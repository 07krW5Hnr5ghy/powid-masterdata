package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderStockDTO;
import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrderStock;
import com.proyect.masterdata.services.IOrderStockItem;
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
@CrossOrigin({"*"})
@RequestMapping("order-stock")
@AllArgsConstructor
public class OrderStockController {

    private final IOrderStock iOrderStock;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:ORDER_STOCK_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") Long orderId,
            @RequestParam("warehouse") String warehouse,
            @RequestBody() List<RequestOrderStockItem> requestOrderStockItemList,
            @RequestParam("tokenUser") String tokenUser
            ) throws InternalErrorExceptions, BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderStock.saveAsync(orderId,warehouse, requestOrderStockItemList,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STOCK_GET')")
    public ResponseEntity<Page<OrderStockDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "warehouses",required = false) List<String> warehouses,
            @RequestParam(value = "orderId",required = false) Long orderId,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderStockDTO>> result = iOrderStock.list(
                user,
                orderId,
                warehouses,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STOCK_GET')")
    public ResponseEntity<List<OrderStockDTO>> listOrderStock(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderStockDTO>> result = iOrderStock.listOrderStock(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STOCK_GET')")
    public ResponseEntity<List<OrderStockDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderStockDTO>> result = iOrderStock.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
