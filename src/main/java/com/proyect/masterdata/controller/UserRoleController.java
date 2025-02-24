package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IUserRole;

import lombok.AllArgsConstructor;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("user-role")
@AllArgsConstructor
public class UserRoleController {

    private final IUserRole iUserRole;

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:USER_ROLE_POST')")
    ResponseEntity<ResponseSuccess> save(
            @RequestParam(value = "username") String username,
            @RequestParam(value = "role") String role,
            @RequestParam(value = "tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iUserRole.saveAsync(username, role, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
        //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:USER_ROLE_POST')")
    ResponseEntity<ResponseDelete> delete(
            @RequestParam(value = "username") String username,
            @RequestParam(value = "role") String role,
            @RequestParam(value = "tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iUserRole.delete(username, role, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
        //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:USER_ROLE_POST')")
    ResponseEntity<ResponseSuccess> activate(
            @RequestParam(value = "username") String username,
            @RequestParam(value = "role") String role,
            @RequestParam(value = "tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iUserRole.activate(username, role, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
