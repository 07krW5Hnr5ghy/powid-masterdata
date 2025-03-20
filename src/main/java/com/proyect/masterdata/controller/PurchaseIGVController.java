package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PurchaseIGVDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseIGV;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IPurchaseIGV;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase-discount")
@AllArgsConstructor
public class PurchaseIGVController {
    private final IPurchaseIGV iPurchaseIGV;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestPurchaseIGV requestPurchaseIGV
            ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchaseIGV.saveAsync(requestPurchaseIGV);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<List<PurchaseIGVDTO>> list() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseIGVDTO>> result = iPurchaseIGV.listPurchaseDiscount();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<Page<PurchaseIGVDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "value", required = false) Double value,
            @RequestParam(value = "percentage", required = false) Boolean percentage,
            @RequestParam(value = "status", required = false) Boolean status,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<PurchaseIGVDTO>> result = iPurchaseIGV.list(
                user,
                name,
                value,
                percentage,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize,
                status);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
