package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderStockItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-stock-item")
@AllArgsConstructor
public class OrderStockItemController {

    private final IOrderStockItem iOrderStockItem;

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_STOCK_ITEM_GET')")
    public ResponseEntity<Page<OrderStockItemDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "orderIds", required = false) List<Long> orderIds,
            @RequestParam(value = "warehouses",required = false) List<String> warehouses,
            @RequestParam(value = "productSku",required = false) String productSku,
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderStockItemDTO>> result = iOrderStockItem.list(
                user,
                orderIds,
                warehouses,
                productSku,
                serial,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination/status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_STOCK_ITEM_GET')")
    public ResponseEntity<Page<OrderStockItemDTO>> listFalse(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "orderIds", required = false) List<Long> orderIds,
            @RequestParam(value = "warehouses",required = false) List<String> warehouses,
            @RequestParam(value = "productSku",required = false) String productSku,
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderStockItemDTO>> result = iOrderStockItem.listFalse(
                user,
                orderIds,
                warehouses,
                productSku,
                serial,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_STOCK_ITEM_GET')")
    public ResponseEntity<List<OrderStockItemDTO>> listOrderStockItem(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderStockItemDTO>> result = iOrderStockItem.listOrderStockItem(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_STOCK_ITEM_GET')")
    public ResponseEntity<List<OrderStockItemDTO>> listOrderStockItemFalse(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderStockItemDTO>> result = iOrderStockItem.listOrderStockItem(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") Long orderId,
            @RequestParam("tokenUser") String tokenUser,
            @RequestBody()RequestOrderStockItem requestOrderStockItem
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderStockItem.save(orderId,requestOrderStockItem,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProduct,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iOrderStockItem.delete(orderId,supplierProduct,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<ResponseSuccess> update(
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProduct,
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("quantity") Integer quantity
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderStockItem.update(orderId,supplierProduct,tokenUser,quantity);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PostMapping("activate")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProduct,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderStockItem.activate(orderId,supplierProduct,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
