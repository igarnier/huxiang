pragma solidity ^0.4.24;

contract Pong {

     event PongEvent(uint256);

     function Pong() public {  }

     function recv_ping(uint256 uid_a) public {

          emit PongEvent(uid_a);
     }
}
