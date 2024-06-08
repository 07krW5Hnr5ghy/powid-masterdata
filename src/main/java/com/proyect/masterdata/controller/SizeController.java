package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISize;
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
@RequestMapping("size")
@AllArgsConstructor
public class SizeController {

    private final ISize iSize;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SIZE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("sizeType") String sizeType,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSize.save(name, sizeType, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SIZE_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iSize.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:SIZE_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSize.activate(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:SIZE_GET')")
    public ResponseEntity<List<SizeDTO>> listSize() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SizeDTO>> result = iSize.listSize();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:SIZE_GET')")
    public ResponseEntity<Page<SizeDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<SizeDTO>> result = iSize.list(name, user, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination/status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:SIZE_GET')")
    public ResponseEntity<Page<SizeDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<SizeDTO>> result = iSize.listStatusFalse(name, user, sort, sortColumn,
                pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "size-type")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:SIZE_GET')")
    public ResponseEntity<List<SizeDTO>> findAllSizeTypeName(
            @RequestParam("nameSizeType") String nameSizeType) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SizeDTO>> result = iSize.findAllSizeTypeName(nameSizeType);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
