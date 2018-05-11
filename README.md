# stripe-servant

The Stripe API (partially) defined in Haskell as a Servant API type


### :bangbang: :fire: :warning: :fire: USE [dmjio/stripe](https://github.com/dmjio/stripe) INSTEAD :fire: :warning: :fire: :bangbang:

You're almost certainly looking for [dmjio/stripe](https://github.com/dmjio/stripe), but I started this project because I needed two things that it didn't offer:

1. thorough Stripe Connect support
2. ACH

I also wanted to explore the use of [`servant-client`](https://hackage.haskell.org/package/servant-client) in the process of implementing those, so here we are.

The project I was building this for was eventually canceled though, so it should also be noted that this logic has never been used in anger.

Consider yourself disclaimed!


## Stripe Version

This package currently targets Stripe API version [`2017-08-15`](https://stripe.com/docs/upgrades#2017-08-15) and you can find copies of the documentation for that Stripe version in [`bchase/stripe-docs`](https://github.com/bchase/stripe-docs/tree/master/v2017-08-15).

A `StripeVersion` data type is also provided, but it currently only serves to set the [`Stripe-Version`](https://stripe.com/docs/api#versioning) header appropriately.


## Usage

[`app/Main.hs`](https://github.com/bchase/stripe-servant/blob/master/app/Main.hs) contains some example usage:

```haskell
createAndChargeAndDeleteCustomer :: Stripe (Customer, Charge, [Charge], Bool)
createAndChargeAndDeleteCustomer = do
  cust    <- stripeS WithoutConnect . createCustomer $ custReq
  charge  <- stripeS WithoutConnect . createCharge   $ chargeReq cust

  charges <- stripeL WithoutConnect . paginate [ By 10 ] $ listCharges

  deleted <- fmap stripeDestroyDeleted . stripeD' WithoutConnect . destroyCustomer $ customerId cust

  return (cust, charge, charges, True)

  where
    custReq = (customerCreateReq (Token "tok_visa")) { customerCreateEmail = Just "test@example.com" }

    chargeReq Customer{customerId} = chargeCreateReq (Price USD 10000) (PCustomer customerId)


main :: IO ()
main = do
  let ver    = StripeVersion'2017'08'15
      key    = StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"
      config = StripeConfig ver key

  eResp <- stripeIO config createAndChargeAndDeleteCustomer

  case eResp of
    Left  (StripeErrorResponse   err  ) -> putStrLn "Stripe Error:"     >> print err
    Left  (StripeDecodeFailure   err _) -> putStrLn "Decode Failure:"   >> print err
    Left  (StripeConnectionError err  ) -> putStrLn "Connection Error:" >> print err
    Right (cust, charge, charges, gone) -> print charge
```

This can be compiled and run with [Stack](https://docs.haskellstack.org/en/stable/README/):

```
$ stack build && stack exec stripe-servant-exe
```


### Usage Explanation

Let's take a look at some of these functions step-by-step.

First, we generate `createCharge` with, essentially:

```haskell
type ChargeCreate = -- POST https://api.stripe.com/v1/charges
  "v1" :> "charges"
    :> ReqBody '[FormUrlEncoded] ChargeCreateReq
    :> Header "Stripe-Account" StripeAccountId
    :> Header "Authorization"  StripeSecretKey
    :> Header "Stripe-Version" StripeVersion
    :> Post '[JSON] (StripeScalarResp Charge)

createCharge :: ChargeCreateReq -> StripeClient (StripeScalarResp Charge)
createCharge = Servant.Client.client (Proxy :: Proxy ChargeCreate)
```


But in the actual source, the above collapses to:

```haskell
type ChargeCreate = "v1" :> "charges" :> RBody ChargeCreateReq :> StripeHeaders (PostS Charge)

createCharge :: CreateS ChargeCreateReq Charge
createCharge = Servant.Client.client (Proxy :: Proxy ChargeCreate)


-- using the `type` synonyms...

type StripeHeaders resp =
     Header "Stripe-Account" StripeAccountId
  :> Header "Authorization"  StripeSecretKey
  :> Header "Stripe-Version" StripeVersion
  :> resp

type RBody t = ReqBody '[FormUrlEncoded] t

type PostS a = Post '[JSON] (StripeScalarResp  a)


type CreateS req resp = req -> StripeClient (StripeScalarResp  resp)
```

Here we've used [`Servant.Client.client`](https://hackage.haskell.org/package/servant-client-0.13.0.1/docs/Servant-Client.html#v:client) to generate functions that will provide us with a `StripeClient resp`. Here's the type signature again, for reference:

```haskell
createCharge :: ChargeCreateReq -> StripeClient (StripeScalarResp Charge)
```

This `StripeClient` contains a `StripeScalarResp`, which is the raw Servant [`Headers`](http://hackage.haskell.org/package/servant-0.13.0.1/docs/Servant-API-ResponseHeaders.html#t:Headers), but this eventually gets converted into a `StripeScalar` for us. (More on this in a minute.)

Based on the kind of request, our response type will vary among these three:

```haskell
data StripeScalar a = StripeScalar
  { stripeScalarRequestId :: RequestId
  , stripeScalarData      :: a
  }

data StripeList a = StripeList
  { stripeListRequestId :: RequestId
  , stripeListHasMore   :: Bool
  , stripeListData      :: a
  }

data StripeDestroy id = StripeDestroy
  { stripeDestroyRequestId :: RequestId
  , stripeDestroyDeleted   :: Bool
  , stripeDestroyId        :: id
  }
```

These types contain the metadata for the request, which _always_ includes a `RequestId`, but will also contain more fields for the last two types.

Moving on, we'll get ourselves into the `Stripe` monad:

```haskell
-- if we're interested in the `RequestId` or metadata, we can use these functions:
stripeS' :: StripeConnect -> StripeClient (StripeScalarResp  a) -> Stripe (StripeScalar  a)
stripeL' :: StripeConnect -> StripeClient (StripeListResp    a) -> Stripe (StripeList    a)
stripeD' :: StripeConnect -> StripeClient (StripeDestroyResp a) -> Stripe (StripeDestroy a)

-- otherwise, these will `fmap` us straight to `Stripe a`:
stripeS :: StripeConnect -> StripeClient (StripeScalarResp  a) -> Stripe a
stripeL :: StripeConnect -> StripeClient (StripeListResp    a) -> Stripe a
stripeD :: StripeConnect -> StripeClient (StripeDestroyResp a) -> Stripe a
```

> The [`Stripe` monad](https://github.com/bchase/stripe-servant/blob/72535b1bc776b3b298a00277d9f068a5e6e43bfc/src/Stripe/Types.hs#L181-L182) in this package is modeled closely after [`AppT`](https://github.com/parsonsmatt/servant-persistent/blob/744e3960d23642466d9eca784853ac709e930360/src/Config.hs#L36-L40) from [parsonsmatt/servant-persistent](https://github.com/parsonsmatt/servant-persistent).

And finally, we'll want to use these to actually hit the Stripe API, by running them in `IO`:

```haskell
stripeIO :: StripeConfig -> Stripe a -> IO (Either StripeFailure a)
```

All together now!

```haskell
config :: StripeConfig
config = StripeConfig StripeVersion'2017'08'15 $ StripeSecretKey "sk_test_BQokikJOvBiI2HlWgH4olfQ2"

test :: IO (Either StripeFailure Customer)
test = do
  let req = customerCreateReq $ Token "tok_visa"
  return . stripeIO config . stripeS WithoutConnect . createCustomer $ req
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
