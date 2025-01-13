SELECT 
    b.block_number, 
    b.previous_hash, 
    b.timestamp, 
    b.nonce, 
    b.block_hash, 
    b.transaction_count, 
    COALESCE(
        json_agg(
            json_build_object(
                'transaction_id', t.transaction_id,
                'block_number', t.block_number,
                'transaction_hash', t.transaction_hash,
                'request_key', t.request_key,
                'from_wallet', t.from_wallet,
                'to_wallet', t.to_wallet,
                'from_wallet_balance', t.from_wallet_balance,
                'to_wallet_balance', t.to_wallet_balance,
                'transaction_amount', t.transaction_amount,
                'gas_fee', t.gas_fee,
                'timestamp', t.timestamp,
                'status', t.status,
                'validator_name', t.validator_name,
                'vote_percent', t.vote_percent
            )
        ) FILTER (WHERE t.transaction_id IS NOT NULL), 
        '[]'
    ) AS transactions
FROM blocks b
LEFT JOIN transactions t ON b.block_number = t.block_number
WHERE b.block_number = ?
GROUP BY b.block_number;
