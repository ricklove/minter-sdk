
import { address, nat, unit } from './type-aliases';

type Storage = {
    0: (
        { empty: unit }
        | {
            state: Array<{
                request: {
                    owner: address;
                    token_id: nat;
                };
                balance: nat;
            }>
        }
    );
};

type Methods = {
    default: () => Promise<void>;
    query: (params: {
        fa2: address;
        requests: Array<{
            owner: address;
            token_id: nat;
        }>;
    }) => Promise<void>;
    response: (params: {
        request: {
            owner: address;
            token_id: nat;
        };
        balance: nat;
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };
