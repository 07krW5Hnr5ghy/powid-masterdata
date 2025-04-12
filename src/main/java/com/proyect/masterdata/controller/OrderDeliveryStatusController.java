package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderDeliveryStatusDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderDeliveryStatus;
import com.proyect.masterdata.services.IUtil;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("order-delivery-status")
@AllArgsConstructor
public class OrderDeliveryStatusController {
    private final IOrderDeliveryStatus iOrderDeliveryStatus;
    private final IUtil iUtil;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COLOR_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderDeliveryStatus.save(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COLOR_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iOrderDeliveryStatus.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:MARKETING','ROLE:STOCK') and hasAuthority('ACCESS:COLOR_GET')")
    public ResponseEntity<List<OrderDeliveryStatusDTO>> listOrderDeliveryStatus(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderDeliveryStatusDTO>> result = iOrderDeliveryStatus.listOrderDeliveryStatus(user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping(value = "list")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:MARKETING','ROLE:STOCK') and hasAuthority('ACCESS:COLOR_GET')")
    public ResponseEntity<Page<OrderDeliveryStatusDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam(value = "updateStartDate",required = false) String uStartDate,
            @RequestParam(value = "updateEndDate",required = false) String uEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize,
            @RequestParam(value = "status",required = false) Boolean status) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        OffsetDateTime updateStartDate = iUtil.parseToOffsetDateTime(uStartDate,true);
        OffsetDateTime updateEndDate = iUtil.parseToOffsetDateTime(uEndDate,false);
        CompletableFuture<Page<OrderDeliveryStatusDTO>> result = iOrderDeliveryStatus.list(
                name,
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
    @PutMapping()
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderDeliveryStatus.activate(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("filter")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:MARKETING','ROLE:STOCK') and hasAuthority('ACCESS:COLOR_GET')")
    public ResponseEntity<List<OrderDeliveryStatusDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderDeliveryStatusDTO>> result = iOrderDeliveryStatus.listFilter(user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
