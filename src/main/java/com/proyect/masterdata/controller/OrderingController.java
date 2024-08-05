package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestOrderUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrdering;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.FileUtils;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order")
@AllArgsConstructor
@Log4j2
public class OrderingController {

    private final IOrdering iOrdering;

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:SALES') and hasAuthority('ACCESS:ORDER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestPart("requestOrder") RequestOrderSave requestOrderSave,
            @RequestPart("receipts") MultipartFile[] receipts,
            @RequestParam("tokenUser") String tokenUser
    ) throws InternalErrorExceptions, BadRequestExceptions, ExecutionException, InterruptedException, IOException {
        CompletableFuture<ResponseSuccess> result = iOrdering.saveAsync(requestOrderSave,receipts,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK') and hasAuthority('ACCESS:ORDER_GET')")
    public ResponseEntity<Page<OrderDTO>> list(
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "orderState",required = false) String orderState,
            @RequestParam(value = "courier",required = false) String courier,
            @RequestParam(value = "paymentState",required = false) String paymentState,
            @RequestParam(value = "paymentMethod",required = false) String paymentMethod,
            @RequestParam(value = "saleChannel",required = false) String saleChannel,
            @RequestParam(value = "managementType",required = false) String managementType,
            @RequestParam(value = "storeName",required = false) String storeName,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<OrderDTO>> result = iOrdering.list(orderId,user,orderState,courier,paymentState,paymentMethod,saleChannel,managementType,storeName,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_PUT')")
    public ResponseEntity<ResponseSuccess> update(
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderUpdate requestOrderUpdate,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrdering.updateAsync(orderId,requestOrderUpdate,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK') and hasAuthority('ACCESS:ORDER_GET')")
    public ResponseEntity<List<OrderDTO>> listOrders(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<OrderDTO>> result = iOrdering.listOrder(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("detail")
    public ResponseEntity<OrderDTO> detail(
            @RequestParam("user") String user,
            @RequestParam("id") Long id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<OrderDTO> result = iOrdering.selectOrder(id,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
