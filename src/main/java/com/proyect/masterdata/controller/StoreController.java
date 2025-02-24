package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StoreDTO;
import com.proyect.masterdata.dto.request.RequestStore;
import com.proyect.masterdata.dto.request.RequestStoreSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStore;
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
@RequestMapping("store")
@AllArgsConstructor
public class StoreController {

    private IStore iStore;

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:STORE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestStoreSave requestClientChannelSave,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iStore.saveAsync(requestClientChannelSave, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PutMapping()
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:STORE_PUT')")
    public ResponseEntity<StoreDTO> update(
            @RequestBody() RequestStore requestClientChannel) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<StoreDTO> result = iStore.update(requestClientChannel);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:STORE_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iStore.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:BUSINESS') and hasAuthority('ACCESS:STORE_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iStore.activate(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:STORE_GET')")
    public ResponseEntity<Page<StoreDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<StoreDTO>> result = iStore.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination/status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:STORE_GET')")
    public ResponseEntity<Page<StoreDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<StoreDTO>> result = iStore.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:BUSINESS','ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:STORE_GET')")
    public ResponseEntity<List<StoreDTO>> listStore(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<StoreDTO>> result = iStore.listStore(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
