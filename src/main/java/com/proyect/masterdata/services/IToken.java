package com.proyect.masterdata.services;

import org.springframework.security.core.Authentication;

public interface IToken {

    public String generateJwt(Authentication auth);
}
