package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.SupplyOrderDTO;
import com.proyect.masterdata.dto.request.RequestSupplyOrder;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISupplyOrder;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("supplyOrder")
@AllArgsConstructor
public class SupplyOrderController {
    private final ISupplyOrder iSupplyOrder;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:PURCHASE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestSupplyOrder requestSupplyOrder,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSupplyOrder.saveAsync(requestSupplyOrder, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<Page<SupplyOrderDTO>> list(
            @RequestParam(value = "ref", required = false) String ref,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<SupplyOrderDTO>> result = iSupplyOrder.list(
                ref,
                user,
                warehouse,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<List<SupplyOrderDTO>> listPurchase(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SupplyOrderDTO>> result = iSupplyOrder.listPurchase(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<SupplyOrderDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SupplyOrderDTO>> result = iSupplyOrder.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("check-stock")
    public ResponseEntity<List<CheckStockDTO>> checkStock(
            @RequestParam("user") String user,
            @RequestParam("supplierProductId") UUID supplierProductId
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CheckStockDTO>> result = iSupplyOrder.checkStock(supplierProductId,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
