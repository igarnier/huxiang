pragma solidity ^0.4.24;

contract Mother {

     address service;
     address broker;
     uint256 last_serv_blk;
     uint256 last_brok_blk;
     uint256 timeout;

     event ServiceAlive(uint256);
     event BrokerAlive(uint256);

     event ServiceTimeout(uint256);
     event BrokerTimeout(uint256);

     function Mother(address broker_a, address service_a, uint256 timeout_a) public {
          service = service_a;
          broker  = broker_a;
          timeout = timeout_a;
          last_serv_blk = 0;
          last_brok_blk = 0;
     }

     function service_deposit() public payable {
          require(msg.sender == service);

          require(msg.value == 1000);

          last_brok_blk = block.number;
     }

     function broker_deposit() public payable {
          require(msg.sender == broker);

          require(msg.value == 1000);

          last_serv_blk = block.number;
     }

     function service_keepalive() public {
          require(msg.sender == service);
          
          // service updates its keepalive
          last_serv_blk = block.number;

          // service checks that broker's keepalive is not too old
          uint256 delta = block.number - last_brok_blk;

          if(delta > timeout) {
               // Enact programmable timeout resolution here
               service.transfer(this.balance);
               emit BrokerTimeout(delta);
          } else {
               emit BrokerAlive(delta);
          }
     }

     function broker_keepalive() public {
          require(msg.sender == broker);

          last_brok_blk = block.number;

          uint256 delta = block.number - last_serv_blk;

          if(delta > timeout) {
               broker.transfer(this.balance);
               emit ServiceTimeout(delta);
          } else {
               emit ServiceAlive(delta);
          }
     }

}
