package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ClientDTO;
import com.proyect.masterdata.dto.request.RequestClient;
import com.proyect.masterdata.dto.request.RequestClientSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IClient;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
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
@RequestMapping("client")
@AllArgsConstructor
public class ClientController {
    private IClient iClient;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CLIENT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestClientSave requestClientSave) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iClient.saveAsync(requestClientSave);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PostMapping(value = "clients", consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CLIENT_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestClientSave> requestClientSaveList,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseSuccess result = iClient.saveAll(requestClientSaveList, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CLIENT_PUT')")
    public ResponseEntity<ClientDTO> update(
            @RequestBody() RequestClient requestClient,
            @RequestParam(value = "tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ClientDTO> result = iClient.update(requestClient, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CLIENT_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("ruc") String ruc,
            @RequestParam("user") String user) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iClient.delete(ruc, user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:CLIENT_GET')")
    public ResponseEntity<Page<ClientDTO>> list(
            @RequestParam(value = "ruc", required = false) String ruc,
            @RequestParam(value = "business", required = false) String business,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ClientDTO>> result = iClient.list(ruc, business, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
