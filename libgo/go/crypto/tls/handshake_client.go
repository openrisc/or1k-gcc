// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package tls

import (
	"bytes"
	"crypto"
	"crypto/rsa"
	"crypto/subtle"
	"crypto/x509"
	"errors"
	"io"
	"strconv"
)

func (c *Conn) clientHandshake() error {
	finishedHash := newFinishedHash(versionTLS10)

	if c.config == nil {
		c.config = defaultConfig()
	}

	hello := &clientHelloMsg{
		vers:               maxVersion,
		cipherSuites:       c.config.cipherSuites(),
		compressionMethods: []uint8{compressionNone},
		random:             make([]byte, 32),
		ocspStapling:       true,
		serverName:         c.config.ServerName,
		supportedCurves:    []uint16{curveP256, curveP384, curveP521},
		supportedPoints:    []uint8{pointFormatUncompressed},
		nextProtoNeg:       len(c.config.NextProtos) > 0,
	}

	t := uint32(c.config.time().Unix())
	hello.random[0] = byte(t >> 24)
	hello.random[1] = byte(t >> 16)
	hello.random[2] = byte(t >> 8)
	hello.random[3] = byte(t)
	_, err := io.ReadFull(c.config.rand(), hello.random[4:])
	if err != nil {
		c.sendAlert(alertInternalError)
		return errors.New("short read from Rand")
	}

	finishedHash.Write(hello.marshal())
	c.writeRecord(recordTypeHandshake, hello.marshal())

	msg, err := c.readHandshake()
	if err != nil {
		return err
	}
	serverHello, ok := msg.(*serverHelloMsg)
	if !ok {
		return c.sendAlert(alertUnexpectedMessage)
	}
	finishedHash.Write(serverHello.marshal())

	vers, ok := mutualVersion(serverHello.vers)
	if !ok || vers < versionTLS10 {
		// TLS 1.0 is the minimum version supported as a client.
		return c.sendAlert(alertProtocolVersion)
	}
	c.vers = vers
	c.haveVers = true

	if serverHello.compressionMethod != compressionNone {
		return c.sendAlert(alertUnexpectedMessage)
	}

	if !hello.nextProtoNeg && serverHello.nextProtoNeg {
		c.sendAlert(alertHandshakeFailure)
		return errors.New("server advertised unrequested NPN")
	}

	suite := mutualCipherSuite(c.config.cipherSuites(), serverHello.cipherSuite)
	if suite == nil {
		return c.sendAlert(alertHandshakeFailure)
	}

	msg, err = c.readHandshake()
	if err != nil {
		return err
	}
	certMsg, ok := msg.(*certificateMsg)
	if !ok || len(certMsg.certificates) == 0 {
		return c.sendAlert(alertUnexpectedMessage)
	}
	finishedHash.Write(certMsg.marshal())

	certs := make([]*x509.Certificate, len(certMsg.certificates))
	for i, asn1Data := range certMsg.certificates {
		cert, err := x509.ParseCertificate(asn1Data)
		if err != nil {
			c.sendAlert(alertBadCertificate)
			return errors.New("failed to parse certificate from server: " + err.Error())
		}
		certs[i] = cert
	}

	if !c.config.InsecureSkipVerify {
		opts := x509.VerifyOptions{
			Roots:         c.config.RootCAs,
			CurrentTime:   c.config.time(),
			DNSName:       c.config.ServerName,
			Intermediates: x509.NewCertPool(),
		}

		for i, cert := range certs {
			if i == 0 {
				continue
			}
			opts.Intermediates.AddCert(cert)
		}
		c.verifiedChains, err = certs[0].Verify(opts)
		if err != nil {
			c.sendAlert(alertBadCertificate)
			return err
		}
	}

	if _, ok := certs[0].PublicKey.(*rsa.PublicKey); !ok {
		return c.sendAlert(alertUnsupportedCertificate)
	}

	c.peerCertificates = certs

	if serverHello.ocspStapling {
		msg, err = c.readHandshake()
		if err != nil {
			return err
		}
		cs, ok := msg.(*certificateStatusMsg)
		if !ok {
			return c.sendAlert(alertUnexpectedMessage)
		}
		finishedHash.Write(cs.marshal())

		if cs.statusType == statusTypeOCSP {
			c.ocspResponse = cs.response
		}
	}

	msg, err = c.readHandshake()
	if err != nil {
		return err
	}

	keyAgreement := suite.ka()

	skx, ok := msg.(*serverKeyExchangeMsg)
	if ok {
		finishedHash.Write(skx.marshal())
		err = keyAgreement.processServerKeyExchange(c.config, hello, serverHello, certs[0], skx)
		if err != nil {
			c.sendAlert(alertUnexpectedMessage)
			return err
		}

		msg, err = c.readHandshake()
		if err != nil {
			return err
		}
	}

	var certToSend *Certificate
	var certRequested bool
	certReq, ok := msg.(*certificateRequestMsg)
	if ok {
		certRequested = true

		// RFC 4346 on the certificateAuthorities field:
		// A list of the distinguished names of acceptable certificate
		// authorities. These distinguished names may specify a desired
		// distinguished name for a root CA or for a subordinate CA;
		// thus, this message can be used to describe both known roots
		// and a desired authorization space. If the
		// certificate_authorities list is empty then the client MAY
		// send any certificate of the appropriate
		// ClientCertificateType, unless there is some external
		// arrangement to the contrary.

		finishedHash.Write(certReq.marshal())

		// For now, we only know how to sign challenges with RSA
		rsaAvail := false
		for _, certType := range certReq.certificateTypes {
			if certType == certTypeRSASign {
				rsaAvail = true
				break
			}
		}

		// We need to search our list of client certs for one
		// where SignatureAlgorithm is RSA and the Issuer is in
		// certReq.certificateAuthorities
	findCert:
		for i, cert := range c.config.Certificates {
			if !rsaAvail {
				continue
			}

			leaf := cert.Leaf
			if leaf == nil {
				if leaf, err = x509.ParseCertificate(cert.Certificate[0]); err != nil {
					c.sendAlert(alertInternalError)
					return errors.New("tls: failed to parse client certificate #" + strconv.Itoa(i) + ": " + err.Error())
				}
			}

			if leaf.PublicKeyAlgorithm != x509.RSA {
				continue
			}

			if len(certReq.certificateAuthorities) == 0 {
				// they gave us an empty list, so just take the
				// first RSA cert from c.config.Certificates
				certToSend = &cert
				break
			}

			for _, ca := range certReq.certificateAuthorities {
				if bytes.Equal(leaf.RawIssuer, ca) {
					certToSend = &cert
					break findCert
				}
			}
		}

		msg, err = c.readHandshake()
		if err != nil {
			return err
		}
	}

	shd, ok := msg.(*serverHelloDoneMsg)
	if !ok {
		return c.sendAlert(alertUnexpectedMessage)
	}
	finishedHash.Write(shd.marshal())

	// If the server requested a certificate then we have to send a
	// Certificate message, even if it's empty because we don't have a
	// certificate to send.
	if certRequested {
		certMsg = new(certificateMsg)
		if certToSend != nil {
			certMsg.certificates = certToSend.Certificate
		}
		finishedHash.Write(certMsg.marshal())
		c.writeRecord(recordTypeHandshake, certMsg.marshal())
	}

	preMasterSecret, ckx, err := keyAgreement.generateClientKeyExchange(c.config, hello, certs[0])
	if err != nil {
		c.sendAlert(alertInternalError)
		return err
	}
	if ckx != nil {
		finishedHash.Write(ckx.marshal())
		c.writeRecord(recordTypeHandshake, ckx.marshal())
	}

	if certToSend != nil {
		certVerify := new(certificateVerifyMsg)
		digest := make([]byte, 0, 36)
		digest = finishedHash.serverMD5.Sum(digest)
		digest = finishedHash.serverSHA1.Sum(digest)
		signed, err := rsa.SignPKCS1v15(c.config.rand(), c.config.Certificates[0].PrivateKey.(*rsa.PrivateKey), crypto.MD5SHA1, digest)
		if err != nil {
			return c.sendAlert(alertInternalError)
		}
		certVerify.signature = signed

		finishedHash.Write(certVerify.marshal())
		c.writeRecord(recordTypeHandshake, certVerify.marshal())
	}

	masterSecret := masterFromPreMasterSecret(c.vers, preMasterSecret, hello.random, serverHello.random)
	clientMAC, serverMAC, clientKey, serverKey, clientIV, serverIV :=
		keysFromMasterSecret(c.vers, masterSecret, hello.random, serverHello.random, suite.macLen, suite.keyLen, suite.ivLen)

	clientCipher := suite.cipher(clientKey, clientIV, false /* not for reading */)
	clientHash := suite.mac(c.vers, clientMAC)
	c.out.prepareCipherSpec(c.vers, clientCipher, clientHash)
	c.writeRecord(recordTypeChangeCipherSpec, []byte{1})

	if serverHello.nextProtoNeg {
		nextProto := new(nextProtoMsg)
		proto, fallback := mutualProtocol(c.config.NextProtos, serverHello.nextProtos)
		nextProto.proto = proto
		c.clientProtocol = proto
		c.clientProtocolFallback = fallback

		finishedHash.Write(nextProto.marshal())
		c.writeRecord(recordTypeHandshake, nextProto.marshal())
	}

	finished := new(finishedMsg)
	finished.verifyData = finishedHash.clientSum(masterSecret)
	finishedHash.Write(finished.marshal())
	c.writeRecord(recordTypeHandshake, finished.marshal())

	serverCipher := suite.cipher(serverKey, serverIV, true /* for reading */)
	serverHash := suite.mac(c.vers, serverMAC)
	c.in.prepareCipherSpec(c.vers, serverCipher, serverHash)
	c.readRecord(recordTypeChangeCipherSpec)
	if err := c.error(); err != nil {
		return err
	}

	msg, err = c.readHandshake()
	if err != nil {
		return err
	}
	serverFinished, ok := msg.(*finishedMsg)
	if !ok {
		return c.sendAlert(alertUnexpectedMessage)
	}

	verify := finishedHash.serverSum(masterSecret)
	if len(verify) != len(serverFinished.verifyData) ||
		subtle.ConstantTimeCompare(verify, serverFinished.verifyData) != 1 {
		return c.sendAlert(alertHandshakeFailure)
	}

	c.handshakeComplete = true
	c.cipherSuite = suite.id
	return nil
}

// mutualProtocol finds the mutual Next Protocol Negotiation protocol given the
// set of client and server supported protocols. The set of client supported
// protocols must not be empty. It returns the resulting protocol and flag
// indicating if the fallback case was reached.
func mutualProtocol(clientProtos, serverProtos []string) (string, bool) {
	for _, s := range serverProtos {
		for _, c := range clientProtos {
			if s == c {
				return s, false
			}
		}
	}

	return clientProtos[0], true
}
