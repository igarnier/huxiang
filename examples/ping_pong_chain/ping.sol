pragma solidity ^0.4.24;

contract Ping {

     uint256 uid;

     event PingEvent(uint256 uid);
     event ErrorDetected();

     function Ping() public {
          uid = 0;
     }
     
     function recv_pong(uint256 uid_a) public {
          if(uid_a != uid) {
               emit ErrorDetected();
          } else {
               uid = uid + 1;

               emit PingEvent(uid);
          }
     }
}
