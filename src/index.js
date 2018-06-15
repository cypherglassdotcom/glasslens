import './css/main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const API_URL = 'https://api.cypherglass.com';
const PRODUCERS_ACCOUNT = 'eosio';
const PRODUCERS_SCOPE = 'eosio';
const PRODUCERS_TABLE = 'producers';
const PRODUCERS_GLOBAL_TABLE = 'global';
const PRODUCERS_LIMIT = 5000;

const eos = Eos({httpEndpoint: API_URL});

const app = Main.embed(document.getElementById('root'));

/**
 * List the producers
 */
app.ports.listProducers.subscribe(async () => {

  // todo: call global and get fields
  // total_producer_vote_weight and total_activated_stake

  const producersList = await eos.getTableRows({
    "json": true,
    "scope": PRODUCERS_SCOPE,
    "code": PRODUCERS_ACCOUNT,
    "table": PRODUCERS_TABLE,
    "limit": PRODUCERS_LIMIT
  }).catch(err => {
    console.error(err);
    app.ports.listProducersFail.send("Fail to list Producers");
  });

  if (producersList && producersList.rows) {
    app.ports.listProducersOk.send(producersList.rows);
  } else {
    app.ports.listProducersFail.send("Empty Producers List");
  }
});

/**
 * Load Chain Info and LIB Block Data used for Transaction Creation
 */
app.ports.getBlockData.subscribe(async () => {

  // Retrieves Chain Info
  const chainInfo = await eos.getInfo({})
    .catch(err => {
      console.error(err);
      app.ports.getBlockDataFail.send("Fail to get Chain Info");
    })

  if (chainInfo) {

    // Retrieves Last Irreversible Block Info
    const lib = await eos.getBlock(chainInfo.last_irreversible_block_num)
      .catch(err => {
        console.error(err);
        app.ports.getBlockDataFail.send("Fail to get Last Irreversible Block Data");
      })

    if (lib) {
      // Prepare relevant Block & Chain data
      app.ports.getBlockDataOk.send({
        chain_id: chainInfo.chain_id,
        block_num: lib.block_num,
        ref_block_prefix: lib.ref_block_prefix
      });
    } else {
      app.ports.getBlockDataFail.send("Empty Last Irreversible Block Data");
    }
  } else {
    app.ports.getBlockDataFail.send("Empty Chain Info Data");
  }
});

registerServiceWorker();
