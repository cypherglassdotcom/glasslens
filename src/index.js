import './css/main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

//const API_URL = 'https://api.cypherglass.com';
const API_URL = 'http://199.27.232.138:8888';
const PRODUCERS_ACCOUNT = 'eosio';
const PRODUCERS_SCOPE = 'eosio';
const PRODUCERS_TABLE = 'producers';
const PRODUCERS_GLOBAL_TABLE = 'global';
const PRODUCERS_LIMIT = 5000;
const VOTE_TRANSACTION_EXPIRE_SECS = 600;

const eos = Eos({httpEndpoint: API_URL});

const app = Main.embed(document.getElementById('root'));

/**
 * List the producers
 */
app.ports.listProducers.subscribe(async () => {

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
    }
  }
});

/**
 * Transaction Signer
 */
app.ports.signTransaction.subscribe(async (params) => {
  let [ pk, pkAccount, blockNum, refBlockPrefix, chainId, producers ] = params;

  const expiration = new Date(new Date().getTime() + VOTE_TRANSACTION_EXPIRE_SECS * 1000).toISOString().split('.')[0];

  const refBlockNum = blockNum & 0xFFFF;

  const headers = {
    ref_block_num: refBlockNum,
    ref_block_prefix: refBlockPrefix,
    net_usage_words: 0,
    max_cpu_usage_ms: 0,
    delay_sec: 0,
    expiration
  }

  let eos = Eos({
    keyProvider: pk,
    transactionHeaders: (_, cb) => {
      cb(null, headers);
    },
    broadcast: false,
    sign: true,
    chainId: chainId
  });

  const options = {
    broadcast: false,
    sign: true,
    authorization: pkAccount
  }

  const transaction = await eos.transaction(tr => {
    tr.voteproducer({
      voter: pkAccount,
      proxy: '',
      producers
    });
  }, options).catch(err => {
    console.error(err);

    let errorMsg = 'Fail to Sign Transaction, please make sure you entered a correct Private Key and Account Name';

    if (err && err.message &&
      err.message.indexOf('permission_level.actor') >= 0) {
      errorMsg = `Invalid account name ${pkAccount}`;
    } else if (err && err.name === 'AssertionError' &&
      err.message === 'Invalid public key') {
      errorMsg = 'Incorrect Private Key, could not find Matching Public Key';
    } else if (err && err.name === 'AssertionError' &&
      err.message) {
      errorMsg = err.message;
    }

    app.ports.signTransactionFail.send(errorMsg);
  });

  if (transaction) {
    const transactionJson = JSON.stringify(transaction.transaction, null, 2);
    app.ports.signTransactionOk.send(transactionJson);
  }

  // force cleaning
  params = pk = eos = null;
});

/**
 * Push Transaction
 */
app.ports.pushTransaction.subscribe(async(transactionJson) => {
  const transaction = JSON.parse(transactionJson);

  const transactionResult = await Eos({httpEndpoint: API_URL, broadcast: true})
    .pushTransaction(transaction)
    .catch(err => {
      console.error(err);

      let errorMsg = 'Fail to Submit Transaction';

      if (err && err.name === 'AssertionError' &&
        err.message) {
        errorMsg = err.message;
      } else if (err && err.message) {
        try {
          const response = JSON.parse(err.message);

          if (response && response.error && response.error.details && response.error.details.length) {

            const message = response.error.details[0].message;

            if (message.indexOf('authorizing actor') > 0 && message.indexOf('does not exist')) {
              errorMsg += ' - Entered account name does not exist, please restart the process and make sure you enter the correct account name for the provided key.';
            } else if (message.indexOf('transaction declares authority') >= 0 &&
            message.indexOf('does not have signatures') > 0) {
              errorMsg += ' - Provided key has no authorization to sign for account entered. Please restart the process and make sure you enter the correct key for account entered.';
            } else {
              errorMsg += ' - ' + (response.error.details[0].message || 'Unknown Details');
            }
          } else if (response && response.error) {
            errorMsg += ' - Error Code: ' + response.error.code;
          } else {
            errorMsg += ' - Uknown EOS Error';
          }

        } catch(_) {
          errorMsg += ' - Unknown Reason'
        }
      }

      app.ports.pushTransactionFail.send(errorMsg);
    });

  if (transactionResult) {
    app.ports.pushTransactionOk.send(transactionResult.transaction_id);
  }
});

/**
 * Listener for Online/Offline Network Connection
 */
window.addEventListener('load', () => {

  const updateOnlineStatus = () => {
    const isConnected = navigator.onLine
    app.ports.isNetworkOnline.send(isConnected)
  };

  window.addEventListener('online', updateOnlineStatus);
  window.addEventListener('offline', updateOnlineStatus);

  updateOnlineStatus();
});

registerServiceWorker();
