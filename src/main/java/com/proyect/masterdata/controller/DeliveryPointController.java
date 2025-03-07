package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DeliveryPointDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IDeliveryPoint;
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
@CrossOrigin({"*"})
@RequestMapping("delivery-point")
@AllArgsConstructor
public class DeliveryPointController {
    private final IDeliveryPoint iDeliveryPoint;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("address") String address,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDeliveryPoint.saveAsync(name,address,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COLOR_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iDeliveryPoint.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COLOR_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDeliveryPoint.activate(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<List<DeliveryPointDTO>> listDeliveryPoint() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<DeliveryPointDTO>> result = iDeliveryPoint.listDeliveryPoints();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("filter")
    public ResponseEntity<List<DeliveryPointDTO>> listFilter() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<DeliveryPointDTO>> result = iDeliveryPoint.listFilter();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping(value = "list")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:MARKETING','ROLE:STOCK') and hasAuthority('ACCESS:COLOR_GET')")
    public ResponseEntity<Page<DeliveryPointDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<DeliveryPointDTO>> result = iDeliveryPoint.list(
                name,
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
    @GetMapping(value = "status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:MARKETING','ROLE:STOCK') and hasAuthority('ACCESS:COLOR_GET')")
    public ResponseEntity<Page<DeliveryPointDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<DeliveryPointDTO>> result = iDeliveryPoint.listFalse(
                name,
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
}
