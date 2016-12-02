-module(broker_crypto).
-export([build_envelope/2, extract_message/2]).

build_envelope(RecipientKey, Message) ->
    AESKey = crypto:strong_rand_bytes(16),
    CAESKey = public_key:encrypt_public(AESKey, RecipientKey),
    CMessage = crypto:block_encrypt(aes_ecb, AESKey, pad_aes_data(Message)),
    {CAESKey, CMessage}.

extract_message({CAESKey, CMessage}, PrivKey) ->
    AESKey = public_key:decrypt_private(CAESKey, PrivKey),
    PadMessage = crypto:block_decrypt(aes_ecb, AESKey, CMessage),
    unpad_aes_data(PadMessage).
    

pad_aes_data(Data) ->
    Pad = case byte_size(Data) rem 16 of
              0 -> 64;
              _Else -> 16 - (byte_size(Data) rem 16)
          end,
    [Data, binary:copy(<<Pad:8>>, Pad)].

unpad_aes_data(Data) ->
    Pad = binary:last(Data),
    binary:part(Data, 0, byte_size(Data) - Pad).
