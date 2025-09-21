#!/bin/bash

# Send test messages to Kafka transactions topic
echo "Sending test messages to Kafka..."

for i in {1..100}; do
    echo '{"transactionId": "stream-txn-'$i'", "clientId": "CLIENT_'$(($i % 3 + 1))'", "transactionCode": "APPLICATION", "creditScore": '$(($i % 800 + 600))', "income": '$(($i % 100000 + 40000))', "amount": '$(($i % 20000 + 5000))'}' | \
    docker exec -i rules-engine-kafka kafka-console-producer --bootstrap-server localhost:9092 --topic transactions
done

echo "Sent 100 test messages to Kafka"