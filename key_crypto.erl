% key_crypto.erl
% Aubrey Anderson
% 5 December 2015
%
% This module provides public key crypto methods to 
% encrpyt a message using RSA and AES.
-module(key_crypto).
-export([build_envelope/2, extract_message/2]).

% Build an encrpyted message.
% Return a tuple of an AES key encrypted with the recipient's public key and the
% message encrypted with the AES key.
% Message should be extracted using extract_message/2.
build_envelope(RecipientKey, Message) ->
    % 128-bit AES key
    AESKey = crypto:strong_rand_bytes(16),
    CAESKey = public_key:encrypt_public(AESKey, RecipientKey),
    CMessage = crypto:block_encrypt(aes_ecb, AESKey, pad_aes_data(Message)),
    {CAESKey, CMessage}.

% Extract a message created using build_envelope/2
% Returns the decrypted message using PrivKey.
extract_message({CAESKey, CMessage}, PrivKey) ->
    AESKey = public_key:decrypt_private(CAESKey, PrivKey),
    PadMessage = crypto:block_decrypt(aes_ecb, AESKey, CMessage),
    unpad_aes_data(PadMessage).

% Pad the Data to 128 bits using PKCS7
pad_aes_data(Data) ->
    Pad = case byte_size(Data) rem 16 of
              0 -> 64; % Always need to pad
              _Else -> 16 - (byte_size(Data) rem 16)
          end,
    [Data, binary:copy(<<Pad:8>>, Pad)].

% Unpad padded Data using PKCS7
unpad_aes_data(Data) ->
    Pad = binary:last(Data),
    binary:part(Data, 0, byte_size(Data) - Pad).
