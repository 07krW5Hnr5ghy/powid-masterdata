package com.proyect.masterdata.controller;

import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.GeneralStockDTO;
import com.proyect.masterdata.dto.WarehouseStockDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IGeneralStock;

import lombok.AllArgsConstructor;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("general-stock")
@AllArgsConstructor
public class GeneralStockController {

    private final IGeneralStock iGeneralStock;

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK') and hasAuthority('ACCESS:GENERAL_STOCK_GET')")
    public ResponseEntity<Page<GeneralStockDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "serial",required = false) String serial,
            @RequestParam(value = "productSku",required = false) String productSku,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<GeneralStockDTO>> result = iGeneralStock.list(
                user,
                serial,
                productSku,
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
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK') and hasAuthority('ACCESS:GENERAL_STOCK_GET')")
    public ResponseEntity<List<GeneralStockDTO>> list(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<GeneralStockDTO>> result = iGeneralStock.listGeneralStock(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
