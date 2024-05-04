package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CancelledOrderDTO;
import com.proyect.masterdata.dto.request.RequestCancelledOrder;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICancelledOrder;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("cancelled-order")
@AllArgsConstructor
public class CancelledOrderController {

    private final ICancelledOrder iCancelledOrder;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLES:CUSTOMER_SERVICE') and hasAuthority('ACCESS:CANCELLED_ORDER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestCancelledOrder requestCancelledOrder,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iCancelledOrder.save(requestCancelledOrder,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLES:SALES','ROLE:CUSTOMER_SERVICE','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CANCELLED_ORDER_GET')")
    public ResponseEntity<Page<CancelledOrderDTO>> list(
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "user") String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<CancelledOrderDTO>> result = iCancelledOrder.list(orderId,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
