package com.proyect.masterdata.services;

import org.springframework.security.core.Authentication;

public interface IToken {
    String generateJwt(Authentication auth);
}
