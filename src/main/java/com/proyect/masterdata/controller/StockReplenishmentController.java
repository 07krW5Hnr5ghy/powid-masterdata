package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockReplenishmentDTO;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockReplenishment;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-replenishment")
@AllArgsConstructor
public class StockReplenishmentController {
    private final IStockReplenishment iStockReplenishment;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") UUID orderId,
            @RequestBody() List<RequestStockReplenishmentItem> requestStockReplenishmentItems,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iStockReplenishment.saveAsync(orderId,requestStockReplenishmentItems,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_GET')")
    public ResponseEntity<Page<StockReplenishmentDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "orderIds",required = false) List<UUID> orderIds,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<StockReplenishmentDTO>> result = iStockReplenishment.list(user,orderIds,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("filter")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_GET')")
    public ResponseEntity<List<StockReplenishmentDTO>> listStockReplenishment(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<StockReplenishmentDTO>> result = iStockReplenishment.listStockReplenishment(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
