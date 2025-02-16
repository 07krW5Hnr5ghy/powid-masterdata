package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestModel;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.ModelDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IModel;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("model")
@AllArgsConstructor
public class ModelController {

    private final IModel iModel;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:MODEL_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestModel requestModel
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iModel.saveAsync(requestModel);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:MODEL_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("sku") String sku,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iModel.delete(sku, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iModel.activate(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:MARKETING','ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:MODEL_GET')")
    public ResponseEntity<Page<ModelDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "brands", required = false) List<String> brands,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ModelDTO>> result = iModel.list(
                user,
                name,
                brands,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination-status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:MARKETING','ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:MODEL_GET')")
    public ResponseEntity<Page<ModelDTO>> listStatusFalse(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "brands", required = false) List<String> brands,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ModelDTO>> result = iModel.listStatusFalse(
                user,
                name,
                brands,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:MARKETING','ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:MODEL_GET')")
    public ResponseEntity<List<ModelDTO>> listModels(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ModelDTO>> result = iModel.listModels(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:MARKETING','ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:MODEL_GET')")
    public ResponseEntity<List<ModelDTO>> listModelsFalse(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ModelDTO>> result = iModel.listModelsFalse(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("brand")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:MARKETING','ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:MODEL_GET')")
    public ResponseEntity<List<ModelDTO>> listModelsBrand(
            @RequestParam("user") String user,
            @RequestParam("brand") String brand
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ModelDTO>> result = iModel.listModelBrand(user,brand);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:MARKETING','ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:MODEL_GET')")
    public ResponseEntity<List<ModelDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ModelDTO>> result = iModel.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
