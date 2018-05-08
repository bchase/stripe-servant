# stripe-servant

The Stripe API (partially) defined in Haskell as a Servant API type


# !!! WARNING !!! USE [dmjio/stripe](https://github.com/dmjio/stripe) INSTEAD !!!

You're almost certainly looking for [dmjio/stripe](https://github.com/dmjio/stripe), but I started this project because I needed two things that it didn't offer:

1. thorough Stripe Connect support
2. ACH

And because I decided I wanted to explore the use of `servant-client` in the process of implementing those.

Also, the project I was building this for was eventually canceled, and so it should be noted that this logic has not been used in anger in production.

Consider yourself disclaimed!


## Usage

There aren't currently any tests, but running `app/Main.hs` should result in output to prove that things generally work as intended:

```
$ stack build && stack exec stripe-client-exe
```


## Resources

### Charges

- [X] create
- [X] read
- [X] update
- [X] list
- [X] capture

### Customers

- [X] create
- [X] read
- [X] update
- [X] destroy
- [X] list

### Cards

- [X] create
- [X] read
- [X] update
- [X] destroy
- [X] list

### Bank Accounts

- [X] create
- [X] read
- [X] update
- [X] destroy
- [X] list
- [X] verify

### Plans

- [X] create
- [X] read
- [X] update
- [X] destroy
- [X] list
