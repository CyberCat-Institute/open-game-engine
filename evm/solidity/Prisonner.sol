
pragma solidity ^0.8.19;

contract Prison {

    address prisoner1;
    address prisoner2;
    bool prisoner1Defect;
    bool prisoner2Defect;
    bool prisoner1Played;
    bool prisoner2Played;

    function check() public {
        if (prisoner1Played && prisoner2Played) {
            // if they both defect, they get a small prize
            if (prisoner1Defect && prisoner2Defect) {
                (bool res1, ) = prisoner1.call{value: 2000}("");
                (bool res2, ) = prisoner2.call{value: 2000}("");
                require(res1, "transfer 1 failed!");
                require(res2, "transfer 2 failed!");
            }
            // if prisoner1 defect but prisoner2 cooperates
            // prisonner1 gets a large prize and prisonner2 nothing
            else if (prisoner1Defect && !prisoner2Defect) {
                (bool res2, ) = prisoner1.call{value: 6000}("");
                require(res2, "transfer 2 failed!");
            }
            // if prisoner2 defects but prisoner1 cooperates
            // prisonner2 gets a large prize and prisoner1 nothing
            else if (!prisoner1Defect && prisoner2Defect) {
                (bool res1, ) = prisoner2.call{value: 6000}("");
                require(res1, "transfer 1 failed!");
            }
            // if both prisoners cooperate they both get 4000
            else {
                (bool res1, ) = prisoner1.call{value: 4000}("");
                (bool res2, ) = prisoner2.call{value: 4000}("");
                require(res1, "transfer 1 failed!");
                require(res2, "transfer 2 failed!");
            }
        }
    }

    function cooperate() public {
      if (!prisoner1Played) {
          prisoner1 = msg.sender;
          prisoner1Defect = false;
          prisoner1Played = true;
          check();
      } else if (!prisoner2Played) {
          prisoner2 = msg.sender;
          prisoner2Defect = false;
          prisoner2Played = true;
          check();
      }
    }

    function defect() public {
      if (!prisoner1Played) {
          prisoner1 = msg.sender;
          prisoner1Defect = true;
          prisoner1Played = true;
          check();
      } else if (!prisoner2Played) {
          prisoner2 = msg.sender;
          prisoner2Defect = true;
          prisoner2Played = true;
          check();
      }
    }
}
