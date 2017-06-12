namespace Bfeth

//https://github.com/ethereum/wiki/wiki/RLP

module Item = 

    open System
    open System.Text
    open System.Collections
    open System.Collections.Generic

    let stringEncoder = Encoding.GetEncoding "iso-8859-1"

    type GenericList = System.Collections.Generic.List<obj>

    type Item =             
        | Single of string  // String
        | List of Item list // stringy list
        | NList of Item list //numeric list
        | GList of Item list //generic list

    // Converts an Item into a byte list [this function is ludicrous - and the name is completely unreasonable]
    let toBytes item = 
        let rec loop item (inner:bool) = 
            match item with
                | Single s -> stringEncoder.GetBytes s |> List.ofArray
                // TODO: temp fix: cheating here adding one more byte every time we find a non numeric list in our item and removing one byte
                // when counting the size of the generic bytes
                | GList l -> l |> List.map (fun x-> 0x01uy::(loop x true)) |> List.concat |> List.tail
                // TODO: temp fix: cheating here adding one more byte every time we find a non numeric list in our item
                | List l -> l |> List.map (fun x-> 0x01uy::(loop x true)) |> List.concat
                | NList l -> l |> List.map (fun x-> loop x true) |> List.concat
                in loop item false

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
            | :? bool as b -> if b then "\x01" |> Single else Single ""
            | :? Item as i -> i
            | :? string as s -> s |> stringEncoder.GetBytes |> stringEncoder.GetString  |> Single
            | :? byte as b -> [|b|] |> stringEncoder.GetString |> Single
            | :? bigint as i -> i |> IntNormalizer.toByteArray |> stringEncoder.GetString |> Single
            | :? uint32 as i -> i |> IntNormalizer.toByteArray |> stringEncoder.GetString |> Single
            | :? uint64 as i -> i |> IntNormalizer.toByteArray |> stringEncoder.GetString |> Single
            | :? (bigint list) as l -> l |> List.map IntNormalizer.toByteArray |> Array.concat |> Seq.toList |> List.map ofObject |> NList
            | :? (uint32 list) as l -> l |> List.map IntNormalizer.toByteArray |> Array.concat |> Seq.toList |> List.map ofObject |> NList
            | :? (uint64 list) as l -> l |> List.map IntNormalizer.toByteArray |> Array.concat |> Seq.toList |> List.map ofObject |> NList
            | :? (byte list) as l -> l |> List.toArray |> stringEncoder.GetString |> Single
            | :? GenericList as gl -> gl |> Seq.cast<_> |> Seq.toList |> List.map ofObject |> Seq.toList |> GList  
            | :? IEnumerable as en -> en |> Seq.cast<_> |> Seq.toList |> List.map ofObject |> Seq.toList |> List            
            | x -> if o = null then Single ""
                   else if Microsoft.FSharp.Reflection.FSharpType.IsTuple(x.GetType()) 
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


    let rec encodez (o:obj) = 
        let item = ofObject o in      
        match item with 
            | Single _ -> 
                let b = toBytes item in 
                let length = (List.length b) in
                let lenghtInByte = byte length in
                let lenghtInByteArray = IntNormalizer.toByteArray length |> Array.toList in
                let lenghtOfLenghtBinaryFormInByte = getNumberOfBytesComposingStringBinaryRepresentation length in
                match length with
                    | 0 -> [0x80uy]
                    | 1 when List.head b <= 0x7Fuy -> [List.head b] 
                    | n when n < 56 -> (0x80uy + lenghtInByte) :: b
                    | x -> (0xB7uy + lenghtOfLenghtBinaryFormInByte) :: (lenghtInByteArray @ b)
            | GList i | NList i | List i -> 
                let b = toBytes item in 
                let length = (List.length b) in
                let lenghtInByte = byte length in
                let lenghtInByteArray = IntNormalizer.toByteArray length |> Array.toList in
                let lenghtOfLenghtBinaryFormInByte = getNumberOfBytesComposingStringBinaryRepresentation length in 
                match i with
                    | [] -> [0xC0uy]
                    | [x] -> [ 0xC0uy + lenghtInByte ] @ encodez x
                    | h::t as list ->  
                        let serializedList = list  |> List.map encodez |> List.concat in 
                        match length with
                            | n when n < 56  -> [ 0xC0uy + lenghtInByte ] @ serializedList
                            | _ -> (0xF7uy + lenghtOfLenghtBinaryFormInByte) :: (lenghtInByteArray @ serializedList)



    let rec encode (o:obj) = let encoded = encodez o in let s = List.fold (fun acc x-> acc + sprintf "%02X" x) "" encoded in printfn "%s" s; encoded
