package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PurchasePaymentMethodDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPurchasePaymentMethod;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase-payment-method")
@AllArgsConstructor
public class PurchasePaymentMethodController {
    private final IPurchasePaymentMethod iPurchasePaymentMethod;

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchasePaymentMethod.save(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iPurchasePaymentMethod.delete(name, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchasePaymentMethod.activate(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_GET')")
    public ResponseEntity<List<PurchasePaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchasePaymentMethodDTO>> result = iPurchasePaymentMethod.listPaymentMethod();
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_PAYMENT_METHOD_GET')")
    public ResponseEntity<Page<PurchasePaymentMethodDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize,
            @RequestParam(value = "status",required = false) Boolean status) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<PurchasePaymentMethodDTO>> result = iPurchasePaymentMethod.list(
                name,
                user,
                sort,
                sortColumn,
                pageNumber,
                pageSize,
                status);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
