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
})

registerServiceWorker();
