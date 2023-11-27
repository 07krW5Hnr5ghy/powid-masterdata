package com.proyect.masterdata.services.impl;

import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.request.RequestOnboarding;
import com.proyect.masterdata.dto.response.ResponseLogin;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAuthentication;
import com.proyect.masterdata.services.IToken;
import com.proyect.masterdata.utils.Constants;

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

    @Override
    public ResponseSuccess registerUser(RequestOnboarding requestOnboarding)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser = false;
        boolean existsDni = false;
        boolean existsEmail = false;
        boolean existsByMobile = false;

        try {
            existsUser = userRepository.existsByUsername(requestOnboarding.getUsername().toUpperCase());
            existsDni = userRepository.existsByDni(requestOnboarding.getDni());
            existsEmail = userRepository.existsByEmail(requestOnboarding.getEmail());
            existsByMobile = userRepository.existsByMobile(requestOnboarding.getMobile());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
        }

        if (existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUserExist);
        }

        if (existsDni) {
            throw new BadRequestExceptions(Constants.ErrorUserDniExist);
        }

        if (existsEmail) {
            throw new BadRequestExceptions(Constants.ErrorUserEmailExist);
        }

        if (existsByMobile) {
            throw new BadRequestExceptions(Constants.ErrorUserMobileExist);
        }
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'registerUser'");
    }
}
