package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderStateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderState;
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
@RequestMapping("order-state")
@AllArgsConstructor
public class OrderStateController {

    private final IOrderState iOrderState;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STATE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("hexColor") String hexColor,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderState.save(name,hexColor, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STATE_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iOrderState.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STATE_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderState.activate(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STATE_GET')")
    public ResponseEntity<List<OrderStateDTO>> listState() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderStateDTO>> result = iOrderState.listState();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("filter")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STATE_GET')")
    public ResponseEntity<List<OrderStateDTO>> listFilter() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderStateDTO>> result = iOrderState.listFilter();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STATE_GET')")
    public ResponseEntity<Page<OrderStateDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderStateDTO>> result = iOrderState.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination/status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STATE_GET')")
    public ResponseEntity<Page<OrderStateDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderStateDTO>> result = iOrderState.listStatusFalse(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
