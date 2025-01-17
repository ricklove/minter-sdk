import { $log } from '@tsed/logger';
import { BigNumber } from 'bignumber.js';
import { bootstrap, TestTz } from '../bootstrap-sandbox';
import { Contract, nat, bytes, address } from '../../src/type-aliases';
import {
  originateEnglishAuctionTez,
  MintNftParam,
  originateNftFaucet
} from '../../src/nft-contracts';
import { TezosToolkit, MichelsonMap } from '@taquito/taquito';

import { TransactionOperation } from '@taquito/taquito/dist/types/operations/transaction-operation';
import {
  OpKind,
  OperationContentsAndResultTransaction,
  OperationResultTransaction
} from '@taquito/rpc';
import {addOperator} from '../../src/fa2-interface'
import {Fa2_tokens, Tokens } from '../../src/auction-interface'

jest.setTimeout(180000); // 3 minutes

describe('test NFT auction', () => {
  let tezos: TestTz;
  let nftAuction: Contract;
  let nftAuctionBob : Contract;
  let nftAuctionAlice : Contract;
  let nftContract : Contract;
  let bobAddress : address;
  let aliceAddress : address;
  let startTime : Date;
  let endTime : Date;
  let tokenId : BigNumber;
  let empty_metadata_map : MichelsonMap<string, bytes>;
  let mintToken : MintNftParam;

  beforeAll(async () => {
    tezos = await bootstrap();
    empty_metadata_map = new MichelsonMap();
    tokenId = new BigNumber(0);
    bobAddress = await tezos.bob.signer.publicKeyHash();
    aliceAddress = await tezos.alice.signer.publicKeyHash();
    mintToken = {
      token_metadata: {
        token_id: tokenId,
        token_info: empty_metadata_map
      },
      owner: bobAddress
    };
  });

  beforeEach(async() => {
    $log.info('originating nft auction...');
    nftAuction = await originateEnglishAuctionTez(tezos.bob);
    nftAuctionBob = await tezos.bob.contract.at(nftAuction.address);
    nftAuctionAlice = await tezos.alice.contract.at(nftAuction.address);

    $log.info('originating nft faucets...');
    nftContract = await originateNftFaucet(tezos.bob, bobAddress);

    $log.info('minting token')

    const opMint = await nftContract.methods.mint([mintToken]).send();
    const hash = await opMint.confirmation();
    $log.info(`Minted tokens. Consumed gas: ${opMint.consumedGas}`);

    $log.info('adding auction contract as operator');
    await addOperator(nftContract.address, tezos.bob, nftAuction.address, tokenId);
    $log.info('Auction contract added as operator');
    
    const fa2_tokens : Fa2_tokens = {
        token_id : tokenId,
        amount : new BigNumber(1)
    }

    const tokens : Tokens = {
        fa2_address : nftContract.address,
        fa2_batch : [fa2_tokens]
    }
    
    startTime = new Date()
    startTime.setSeconds(startTime.getSeconds() + 7)
    endTime = new Date(startTime.valueOf())
    endTime.setHours(endTime.getHours() + 1)
    //opening price = 10 tz, percent raise =10, min_raise = 10tz, round_time = 1 hr, extend_time = 5 mins, end_time = start_time + 1hr, 
    const opAuction = await nftAuctionBob.methods.configure(new BigNumber(10000000), new BigNumber(10), new BigNumber(10000000), new BigNumber(3600), new BigNumber(300), [tokens], startTime, endTime).send({amount : 10});
    await opAuction.confirmation();
    $log.info(`Auction configured. Consumed gas: ${opAuction.consumedGas}`);
  });

  test('bid of less than asking price should fail', async() => {
    $log.info(`Alice bids 9tz expecting it to fail`);
    const failedOpeningBid = nftAuctionAlice.methods.bid(0).send({amount : 9});
    //TODO: test contents of error message
    return expect(failedOpeningBid).rejects.toHaveProperty('errors');
  });
  test('place bid meeting opening price and then raise it by valid amount by min_raise_percent', async () => {
    $log.info(`Alice bids 10tz`);
    const opBid = await nftAuctionAlice.methods.bid(0).send({amount : 10});
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);
    
    $log.info(`Alice bids 11tz, a 10% raise of previous bid but less than a 10tz increase`)
    const opBid2 = await nftAuctionAlice.methods.bid(0).send({amount : 11});
    await opBid2.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid2.amount} mutez`);
  });

  test('place bid meeting opening price and then raise it by valid amount by min_raise', async () => {
    $log.info(`Alice bids 200tz`);
    const opBid = await nftAuctionAlice.methods.bid(0).send({amount : 200});
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount} mutez`);
    
    $log.info(`Alice bids 210tz, a 10tz increase but less than a 10% raise of previous bid `)
    const opBid2 = await nftAuctionAlice.methods.bid(0).send({amount : 210});
    await opBid2.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid2.amount} mutez`);
  });

  test('bid too small should fail', async () => {
    $log.info(`Alice bids 20tz`);
    const opBid = await nftAuctionAlice.methods.bid(0).send({amount : 20});
    await opBid.confirmation();
    $log.info(`Bid placed. Amount sent: ${opBid.amount}`);
    $log.info(`Alice bids 21tz and we expect it to fail`);
    const smallBidPromise = nftAuctionAlice.methods.bid(0).send({amount : 21});
    return expect(smallBidPromise).rejects.toHaveProperty('errors' );
  });
});
