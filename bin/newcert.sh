#!/bin/sh

if [ -z "$1" ]
then
    echo "usage: newcert.sh <hostname>" 2>&1
    exit 1
fi

openssl req -new -x509 -days 365 -nodes \
  -subj "/CN=$1" -out "$1.pem" -keyout "$1.pem"

echo "Key and certificate for $1 written stored in $1.pem"
