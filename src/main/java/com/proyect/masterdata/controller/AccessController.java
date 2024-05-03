package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.AccessDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IAccess;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("access")
@AllArgsConstructor
public class AccessController {

    private final IAccess iAccess;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ACCESS_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam(value = "name") String name,
            @RequestParam(value = "tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iAccess.saveAsync(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ACCESS_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam(value = "name") String name,
            @RequestParam(value = "tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseDelete result = iAccess.delete(name,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ACCESS_GET')")
    public ResponseEntity<List<AccessDTO>> list() throws BadRequestExceptions {
        List<AccessDTO> result = iAccess.list();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ACCESS_GET')")
    public ResponseEntity<List<AccessDTO>> listFalse() throws BadRequestExceptions {
        List<AccessDTO> result = iAccess.listFalse();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ACCESS_PUT')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam(value = "name") String name,
            @RequestParam(value = "tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iAccess.activate(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}