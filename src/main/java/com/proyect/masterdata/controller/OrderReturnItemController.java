package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrderReturnItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("order-return-item")
@AllArgsConstructor
public class OrderReturnItemController {
    private final IOrderReturnItem iOrderReturnItem;
    @GetMapping()
    private ResponseEntity<List<OrderReturnItemDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "orderId",required = false) Long orderId
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderReturnItemDTO>> result = iOrderReturnItem.list(user,orderId);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PostMapping()
    private ResponseEntity<ResponseSuccess> save(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderReturnItem requestOrderReturnItem
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderReturnItem.save(orderId,requestOrderReturnItem,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    private ResponseEntity<ResponseDelete> delete(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProductSerial
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iOrderReturnItem.delete(orderId,supplierProductSerial,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @PutMapping()
    private ResponseEntity<ResponseSuccess> update(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProductSerial,
            @RequestParam("quantity") Integer quantity
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderReturnItem.update(orderId,supplierProductSerial,quantity,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @PostMapping("activate")
    private ResponseEntity<ResponseSuccess> activate(
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProductSerial
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderReturnItem.activate(orderId,supplierProductSerial,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("pagination")
    private ResponseEntity<Page<OrderReturnItemDTO>> listPagination(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "orderIds",required = false) List<Long> orderIds,
            @RequestParam(value = "products",required = false) List<String> products,
            @RequestParam(value = "supplierProducts",required = false) List<String> supplierProducts,
            @RequestParam(value = "warehouses",required = false) List<String> warehouses,
            @RequestParam(value = "returnTypes",required = false) List<String> returnTypes,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderReturnItemDTO>> result = iOrderReturnItem.listPagination(
                user,
                orderIds,
                products,
                supplierProducts,
                warehouses,
                returnTypes,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("false")
    private ResponseEntity<Page<OrderReturnItemDTO>> listFalse(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "orderIds",required = false) List<Long> orderIds,
            @RequestParam(value = "products",required = false) List<String> products,
            @RequestParam(value = "supplierProducts",required = false) List<String> supplierProducts,
            @RequestParam(value = "warehouses",required = false) List<String> warehouses,
            @RequestParam(value = "returnTypes",required = false) List<String> returnTypes,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) Date updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderReturnItemDTO>> result = iOrderReturnItem.listPagination(
                user,
                orderIds,
                products,
                supplierProducts,
                warehouses,
                returnTypes,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize
        );
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
