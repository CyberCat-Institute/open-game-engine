module Examples.Lido where

contract :: Transaction -> VM -> VM
contract = loadContract ("lido.sol")

tokenExchange ::
  ( buyer : Address,
    sold_id : Int128,
    tokens_sold : UInt256,
    bought_id : Int128,
    tokens_bought : UInt256
  ) ->
  Transaction
tokenExchange = undefined

stake ::
  ( buyer : Address,
    sold_id : Int128,
    tokens_sold : UInt256,
    bought_id : Int128,
    tokens_bought : UInt256
  ) ->
  State VM UInt256

stakingGame =
  [opengame|
  inputs : ;
  feedback : ;
  :----:

  inputs : ;
  feedback : ;
  outputs : ;
  returns : ;

  :----:
  outputs : ;
  returns : ;
|]

-- {"name":"TokenExchange",
-- "inputs":[
-- {"type":"address","name":"buyer","indexed":true},
-- {"type":"int128","name":"sold_id","indexed":false},
-- {"type":"uint256","name":"tokens_sold","indexed":false},
-- {"type":"int128","name":"bought_id","indexed":false},
-- {"type":"uint256","name":"tokens_bought","indexed":false}],
-- "anonymous":false,
-- "type":"event"},
