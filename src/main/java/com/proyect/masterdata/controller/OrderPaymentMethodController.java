package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderPaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentMethod;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderPaymentMethod;
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
@RequestMapping("order-payment-method")
@AllArgsConstructor
public class OrderPaymentMethodController {

    private final IOrderPaymentMethod iOrderPaymentMethod;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderPaymentMethod.save(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping(value = "order-payment-methods", consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> names,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iOrderPaymentMethod.saveAll(names, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iOrderPaymentMethod.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_GET')")
    public ResponseEntity<List<OrderPaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderPaymentMethodDTO>> result = iOrderPaymentMethod.listPaymentMethod();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_GET')")
    public ResponseEntity<Page<OrderPaymentMethodDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderPaymentMethodDTO>> result = iOrderPaymentMethod.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination/status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_GET')")
    public ResponseEntity<Page<OrderPaymentMethodDTO>> listStatusFalse(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderPaymentMethodDTO>> result = iOrderPaymentMethod.listStatusFalse(name, user, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
