package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockReplenishmentItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockReplenishmentItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-replenishment-item")
@AllArgsConstructor
public class StockReplenishmentItemController {
    private final IStockReplenishmentItem iStockReplenishmentItem;
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_ITEM_GET')")
    public ResponseEntity<Page<StockReplenishmentItemDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "orderIds", required = false) List<UUID> orderIds,
            @RequestParam(value = "products", required = false) List<String> products,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<StockReplenishmentItemDTO>> result = iStockReplenishmentItem.list(
                user,
                orderIds,
                products,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("tokenUser") String tokenUser,
            @RequestBody()RequestStockReplenishmentItem requestStockReplenishmentItem
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iStockReplenishmentItem.add(orderId,requestStockReplenishmentItem,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("productSku") String productSku,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iStockReplenishmentItem.delete(orderId,productSku,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @PutMapping()
    public ResponseEntity<ResponseSuccess> update(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("productSku") String productSku,
            @RequestParam("quantity") Integer quantity,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iStockReplenishmentItem.update(orderId,productSku,quantity,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @PostMapping("activate")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("orderId") UUID orderId,
            @RequestParam("productSku") String productSku,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iStockReplenishmentItem.activate(orderId,productSku,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_ITEM_GET')")
    public ResponseEntity<List<StockReplenishmentItemDTO>> listStockReplenishmentItems(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<StockReplenishmentItemDTO>> result = iStockReplenishmentItem.listStockReplenishmentItem(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
