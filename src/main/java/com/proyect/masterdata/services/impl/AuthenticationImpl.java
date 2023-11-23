package com.proyect.masterdata.services.impl;

import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.response.ResponseLogin;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAuthentication;
import com.proyect.masterdata.services.IToken;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class AuthenticationImpl implements IAuthentication {

    private final AuthenticationManager authenticationManager;
    private final IToken iToken;
    private final UserRepository userRepository;

    public ResponseLogin loginUser(String username, String password) {
        try {

            System.out.println(password);
            System.out.println(username);
            Authentication auth = authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(username.toUpperCase(), password));

            String token = iToken.generateJwt(auth);

            return new ResponseLogin(userRepository.findByUsername(username.toUpperCase()), token);

        } catch (AuthenticationException e) {
            log.error(e);
        }
        return null;
    }
}
