-- Table to store wallet addresses and their balances
CREATE TABLE IF NOT EXISTS users (
    public_key VARCHAR(128) PRIMARY KEY,   -- Unique identifier for a user's wallet
    balance NUMERIC(20, 8) NOT NULL       -- Wallet balance (e.g., cryptocurrency units)
);

-- Table to store details of individual blocks
CREATE TABLE IF NOT EXISTS blocks (
    block_number INT PRIMARY KEY,          -- Unique identifier for a block
    previous_hash VARCHAR(256) NOT NULL,   -- Hash of the previous block (used for chaining)
    block_hash VARCHAR(256) NOT NULL,      -- Unique hash of this block
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP, -- Block creation timestamp
    nonce INT NOT NULL,                    -- Nonce used for proof-of-work or similar mechanisms
    transaction_count INT NOT NULL DEFAULT 0 -- Number of transactions in this block
);

-- Table to store transaction details, linked to blocks
CREATE TABLE IF NOT EXISTS transactions (
    transaction_id BIGINT PRIMARY KEY,    -- Unique identifier for the transaction
    block_number INT NOT NULL,            -- Block to which this transaction belongs
    transaction_hash VARCHAR(256),        -- Unique transaction hash (optional)
    request_key VARCHAR(256),             -- Request key for smart contract transactions (optional)
    from_wallet VARCHAR(128) NOT NULL,    -- Sender's wallet public key
    to_wallet VARCHAR(128),               -- Recipient's wallet public key (nullable for contract calls)
    from_wallet_balance NUMERIC(20, 8),   -- Sender's balance before the transaction
    to_wallet_balance NUMERIC(20, 8),     -- Recipient's balance after the transaction
    transaction_amount NUMERIC(20, 8),    -- Amount transferred in the transaction
    gas_fee NUMERIC(20, 8),               -- Fee for processing the transaction
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP, -- Timestamp of the transaction
    status VARCHAR(10) NOT NULL,          -- Transaction status (e.g., SUCCESS, FAILED, PENDING)
    validator_name VARCHAR(128),          -- Validator that processed this transaction
    vote_percent FLOAT,                   -- Vote percentage during validation
    FOREIGN KEY (block_number) REFERENCES blocks(block_number) ON DELETE CASCADE, -- Links transaction to a block
    FOREIGN KEY (from_wallet) REFERENCES users(public_key), -- Links sender's wallet
    FOREIGN KEY (to_wallet) REFERENCES users(public_key)    -- Links recipient's wallet
);

-- Table to link ETH addresses with Nuchain wallets
CREATE TABLE IF NOT EXISTS eth_nuchain_link (
    nuchain_wallet_id VARCHAR(128) PRIMARY KEY REFERENCES users(public_key),  -- Links to public_key in users table
    eth_address VARCHAR(42) UNIQUE NOT NULL,                                  -- Unique ETH address
    eth_balance NUMERIC(38, 18) DEFAULT 0,                                    -- Balance in Wei
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Table to keep track of the current block number
CREATE TABLE IF NOT EXISTS block_counter (
    current_block_number INT DEFAULT 0 PRIMARY KEY -- Ensures only one row to track the latest block
);

-- Insert the initial row with `current_block_number = 0`
INSERT INTO block_counter (current_block_number)
VALUES (0)
ON CONFLICT DO NOTHING; -- Ensures the row is not inserted if it already exists

-- Retrieve a block and its transactions
-- SELECT b.block_number, b.block_hash, b.timestamp, t.transaction_id, t.transaction_amount
-- FROM blocks b
-- JOIN transactions t ON b.block_number = t.block_number
-- WHERE b.block_number = 1;