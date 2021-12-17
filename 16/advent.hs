import Data.Maybe (mapMaybe, fromJust)

type Bit = Bool

data Packet = Packet
    { version :: Int
    , packetType :: Int
    , contents :: Contents
    } deriving (Show)

data Contents = Operation [Packet] | Literal Int deriving (Show)

parsePacket :: [Bit] -> Maybe (Packet, [Bit])
parsePacket bits =
    let
        (header, contents) = splitAt 6 bits
        (version, type_) = splitAt 3 header
    in
    if all not bits then
        Nothing
    else
        let (parsedContents, restBits) = parseContents (bitsToInt type_) contents
        in
        Just
            ( Packet
                { version = bitsToInt version
                , packetType = bitsToInt type_
                , contents = parsedContents
                }
            , restBits
            )


parseContents :: Int -> [Bit] -> (Contents, [Bit])
parseContents type_ bits =
    if type_ == 4 then
        let
            (parsedLiteral, bitsLeft) = parseLiteral bits
        in
        (Literal parsedLiteral, bitsLeft)
    else
        let
            (parsedOperation, bitsLeft) = parseOperation bits
        in
        (Operation parsedOperation, bitsLeft)

parseLiteral :: [Bit] -> (Int, [Bit])
parseLiteral bits =
    (\(i,r) -> (bitsToInt i, r)) (toGroups bits)
    where
        toGroups bits_ =
            let ((keepGoing:first), rest) = splitAt 5 bits_
            in
            if keepGoing then
                let
                    (r, rr) = toGroups rest
                in
                (first ++ r, rr)
            else
                (first, rest)

parseOperation :: [Bit] -> ([Packet], [Bit])

-- Mode 0: subpackets span a given length
parseOperation (False:rest) =
    let
        (l, contents) = splitAt 15 rest
        totalLength = bitsToInt l
        (subPacketsBits, bitsLeft) = splitAt totalLength contents
    in
    (parseSubPackets subPacketsBits, bitsLeft)

-- Mode 1: there is a given number subpackets
parseOperation (True:rest) =
    let
        (n, contents) = splitAt 11 rest
        nbPackets = bitsToInt n
    in
    parseNSubPackets nbPackets contents


parseSubPackets :: [Bit] -> [Packet]
parseSubPackets bits =
    case (parsePacket bits) of
        Just (firstSubpacket, restBits) ->
            firstSubpacket : parseSubPackets restBits

        Nothing ->
            []

parseNSubPackets :: Int -> [Bit] -> ([Packet], [Bit])
parseNSubPackets nbPackets bits =
    go nbPackets [] bits
    where
        go :: Int -> [Packet] -> [Bit] -> ([Packet], [Bit])
        go 0 packets bits_ = (packets, bits_)
        go n packets bits_ =
            case parsePacket bits_ of
                Just (firstSubpacket, bitsLeft) ->
                    go (n - 1) (packets ++ [firstSubpacket]) bitsLeft

                Nothing ->
                    (packets, bits_)


sumVersions :: Packet -> Int
sumVersions packet =
    case contents packet of
        Literal _ -> version packet
        Operation packets -> version packet + (sum . map sumVersions $ packets)

bitsToInt :: [Bit] -> Int
bitsToInt bits =
    go 0 (reverse bits)
    where
        go :: Int -> [Bit] -> Int
        go _ [] = 0
        go col (first:rest) =
            if first then
                2 ^ col + (go (col + 1) rest)
            else
                go (col + 1) rest

binaryToBits :: String -> [Bit]
binaryToBits binString =
    map (\c -> if c == '1' then True else False) binString

hexToBits :: String -> [Bit]
hexToBits hexString =
    concat $ mapMaybe (\bitChar -> case bitChar of
            '0' -> Just [False,False,False,False]
            '1' -> Just [False,False,False,True]
            '2' -> Just [False,False,True,False]
            '3' -> Just [False,False,True,True]
            '4' -> Just [False,True,False,False]
            '5' -> Just [False,True,False,True]
            '6' -> Just [False,True,True,False]
            '7' -> Just [False,True,True,True]
            '8' -> Just [True,False,False,False]
            '9' -> Just [True,False,False,True]
            'A' -> Just [True,False,True,False]
            'B' -> Just [True,False,True,True]
            'C' -> Just [True,True,False,False]
            'D' -> Just [True,True,False,True]
            'E' -> Just [True,True,True,False]
            'F' -> Just [True,True,True,True]
            _ -> Nothing
        ) hexString

printBits :: [Bit] -> String
printBits bits = map (\bit -> if bit then '1' else '0') bits

main = do
    raw <- getContents
    print . sumVersions . fst . fromJust . parsePacket . hexToBits $ raw