namespace Bfeth

//https://github.com/ethereum/wiki/wiki/RLP

module Item = 

    open System
    open System.Text
    open System.Collections
    open System.Collections.Generic

    let stringEncoder = Encoding.GetEncoding "iso-8859-1"

    type Item =             
        | Single of string          
        | List of Item list

    // Converts an Item into a byte list
    let rec toBytes = function
        | Single s -> stringEncoder.GetBytes s |> List.ofArray
        | List array -> array |> List.map toBytes |> List.concat

    let removeLeadingZeroes (list:byte array) = 
        let list = List.ofArray list in
        let rec loop list prevIsZero =
            match list with
                | [] -> []        
                | h::t -> 
                    if (h=0x0uy && prevIsZero) then loop t true
                    else h :: loop t false
            in Array.ofList (loop list (List.head list = 0x0uy))
  
    let toBigEndian (bytes: byte array) = 
        if BitConverter.IsLittleEndian then 
            Array.rev bytes
        else
            bytes    
   
   // Normalise different type of int, convert the byte representation to big endian and remove leading zeros
    type IntNormalizer =
        static member toByteArray(x:int32) = x |> BitConverter.GetBytes |> toBigEndian |> removeLeadingZeroes
        static member toByteArray(x:int64) = x |> BitConverter.GetBytes |> toBigEndian |> removeLeadingZeroes
        static member toByteArray(x:uint32) = x |> BitConverter.GetBytes |> toBigEndian |> removeLeadingZeroes
        static member toByteArray(x:uint64) = x |> BitConverter.GetBytes |> toBigEndian |> removeLeadingZeroes
        static member toByteArray(x:bigint) = x.ToByteArray() |> toBigEndian |> removeLeadingZeroes

    
    // Convert a generic object into an Item
    let rec ofObject (o:obj) = 
        match o with           
            | :? int32 as i -> i |> IntNormalizer.toByteArray |> stringEncoder.GetString |> Single
            | :? bool as b -> if b then [|0x01uy|] |> stringEncoder.GetString |> Single else Single ""
            | :? Item as i -> i
            | :? string as s -> s |> Single
            | :? byte as b -> [|b|] |> stringEncoder.GetString |> Single
            | :? bigint as i -> i |> IntNormalizer.toByteArray |> stringEncoder.GetString |> Single
            | :? uint32 as i -> i |> IntNormalizer.toByteArray |> stringEncoder.GetString |> Single
            | :? uint64 as i -> i |> IntNormalizer.toByteArray |> stringEncoder.GetString |> Single
            | :? (byte list) as l -> l |> List.toArray |> stringEncoder.GetString |> Single
            | :? IEnumerable as en -> en |> Seq.cast<_> |> Seq.toList |> List.map ofObject |> Seq.toList |> List  
            | x -> if Microsoft.FSharp.Reflection.FSharpType.IsTuple(x.GetType()) 
                    then ofObject (Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields x)
                    else failwith ("Unsupported type: " +  (sprintf "%A" (o.GetType())))
                     
module RLP =
    
    open Item
    open System
    open System.Text
    open System.Collections.Generic

    let rec intToBinary i =
        match i with
            | 0 | 1 -> string i
            | _ ->
                let bit = string (i % 2) in
                (intToBinary (i / 2)) + bit 

    let getNumberOfBytesComposingStringBinaryRepresentation length =    
        let strLen = String.length (intToBinary length) in
        let bytes = byte(strLen/8) in
        let remainder = if strLen % 8 > 0 then 0x01uy else 0x0uy in bytes + remainder

    let rec encode (o:obj) = 
        let item = ofObject o in     
        let b = toBytes item in 
        let length = List.length b in
        let lenghtInByte = byte length in
        let lenghtInByteArray = IntNormalizer.toByteArray length |> Array.toList in
        let lenghtOfLenghtBinaryFormInByte = getNumberOfBytesComposingStringBinaryRepresentation length in
        match item with 
            | Single _ -> 
                match length with
                    | 0 -> [0x80uy]
                    | 1 when List.head b <= 0x7Fuy -> [List.head b] 
                    | n when n < 56 -> (0x80uy + lenghtInByte) :: b
                    | x -> (0xB7uy + lenghtOfLenghtBinaryFormInByte) :: (lenghtInByteArray @ b)
            | List i ->  
                match i with
                    | [] -> [0x80uy]
                    | [x] -> encode x
                    | h::t as list -> 
                        let serializedList =  list  |> List.map encode |> List.concat in 
                        match length with
                            | n when n < 56  -> [ 0xC0uy + lenghtInByte ] @ serializedList
                            | _ -> (0xF7uy + lenghtOfLenghtBinaryFormInByte) :: (lenghtInByteArray @ serializedList)


    let encodeAndPrint (o:obj) = o |> encode |> List.fold (fun acc x-> acc + sprintf "%02X" x) ""
   