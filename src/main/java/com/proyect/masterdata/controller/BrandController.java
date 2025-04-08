package com.proyect.masterdata.controller;

import com.proyect.masterdata.services.IUtil;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.BrandDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IBrand;
import org.springframework.web.bind.annotation.*;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import lombok.AllArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("brand")
@AllArgsConstructor
public class BrandController {

    private final IBrand iBrand;
    private final IUtil iUtil;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:BRAND_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("sku") String sku) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iBrand.saveAsync(name, tokenUser, sku);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:BRAND_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iBrand.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:BRAND_GET')")
    public ResponseEntity<Page<BrandDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "names", required = false) List<String> names,
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam(value = "updateStartDate",required = false) String uStartDate,
            @RequestParam(value = "updateEndDate",required = false) String uEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize,
            @RequestParam(value = "status",required = false) Boolean status) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        OffsetDateTime updateStartDate = iUtil.parseToOffsetDateTime(uStartDate,true);
        OffsetDateTime updateEndDate = iUtil.parseToOffsetDateTime(uEndDate,false);
        CompletableFuture<Page<BrandDTO>> result = iBrand.listPagination(user, names,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate, sort, sortColumn, pageNumber, pageSize,status);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PutMapping()
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:BRAND_PUT')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iBrand.activate(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:BRAND_GET')")
    public ResponseEntity<List<BrandDTO>> listBrands(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<BrandDTO>> result = iBrand.listBrands(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:STOCK','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:BRAND_GET')")
    public ResponseEntity<List<BrandDTO>> listBrandsFalse(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<BrandDTO>> result = iBrand.listBrandsFalse(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<BrandDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException,InterruptedException {
        CompletableFuture<List<BrandDTO>> result = iBrand.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
