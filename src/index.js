import './css/main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));

const API_URL = 'https://api.cypherglass.com';

const eos = Eos({httpEndpoint: API_URL});
eos.getInfo({}).then(result => {
  console.log("EOS GET INFO RESULT", result);

  eos.getBlock(result.last_irreversible_block_num).then(
    result => console.log("EOS LIB RESULT", result)
  )
});

registerServiceWorker();
