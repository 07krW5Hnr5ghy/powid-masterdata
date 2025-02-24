package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.LoginDTO;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.request.RequestLogin;
import com.proyect.masterdata.dto.request.RequestOnboarding;
import com.proyect.masterdata.dto.response.ResponseLogin;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IAuthentication;

import lombok.AllArgsConstructor;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("auth")
@AllArgsConstructor
public class AuthController {

    private final IAuthentication iAuthentication;

    @PostMapping(value = "register")
    public ResponseEntity<ResponseSuccess> register(
            @RequestBody() RequestOnboarding requestOnboarding) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iAuthentication.registerNewClient(requestOnboarding);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping(value = "login" )
    public ResponseEntity<LoginDTO> login(
            @RequestBody() RequestLogin requestLogin) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<LoginDTO> result = iAuthentication.loginUser(requestLogin.getUsername(), requestLogin.getPassword());
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
