package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-item")
@AllArgsConstructor
public class OrderItemController {

    private final IOrderItem iOrderItem;

    @GetMapping("check-stock")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_ITEM_GET')")
    public ResponseEntity<ResponseCheckStockItem> checkStockItem(
            @RequestParam("productSku") String productSku,
            @RequestParam("quantity") Integer quantity,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseCheckStockItem> result = iOrderItem.checkStock(productSku, quantity, user);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_ITEM_POST')")
    public ResponseEntity<ResponseSuccess> addItem(
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderItem requestOrderItem,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderItem.add(orderId,requestOrderItem,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_ITEM_DELETE')")
    public ResponseEntity<ResponseDelete> deleteItem(
            @RequestParam("orderId") Long orderId,
            @RequestParam("productSku") String productSku,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iOrderItem.delete(orderId,productSku,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_ITEM_PUT')")
    public ResponseEntity<ResponseSuccess> updateItem(
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderItem requestOrderItem,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderItem.update(orderId,requestOrderItem,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_ITEM_GET')")
    public ResponseEntity<Page<OrderItemDTO>> listOrderItems(
            @RequestParam("user") String user,
            @RequestParam(value = "orderId",required = false) Long orderId,
            @RequestParam(value = "productSku",required = false) String productSku,
            @RequestParam(value = "quantity",required = false) Integer quantity,
            @RequestParam(value = "discount",required = false) Double discount,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderItemDTO>> result = iOrderItem.listOrderItems(user,orderId,productSku,quantity,discount,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
