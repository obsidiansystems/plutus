with (import ../. { });


let

  db-file = "/tmp/pab-core.db";
  node-port = "3001";

  primary-config = {
    inherit db-file;
    inherit (plutus-pab) client;
    name = "demo-primary";
    webserver-port = "9080";
    walletserver-port = "9081";
    nodeserver-port = "${node-port}";
    chain-index-port = "9083";
    signing-process-port = "9084";
    metadata-server-port = "9085";
    wallet = "1";
  };

in
{

  pab-config = plutus-pab.mkConf primary-config;

}
