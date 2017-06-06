namespace Bfeth

module Temp =

    type Address = byte[]

    type WorldState = int

    type sigma = Sigma of byte[]

    let upsilon sigma tx = 1

    type Account = { 
        nonce: uint64;
        balance: bigint; 
        storageRoot: byte[]; 
        codeHash: byte[]
    }
