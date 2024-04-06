package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderItem;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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
    ) throws BadRequestExceptions {
        ResponseCheckStockItem result = iOrderItem.checkStock(productSku, quantity, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_ITEM_POST')")
    public ResponseEntity<ResponseSuccess> addItem(
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderItem requestOrderItem,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iOrderItem.add(orderId,requestOrderItem,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_ITEM_DELETE')")
    public ResponseEntity<ResponseDelete> deleteItem(
            @RequestParam("orderId") Long orderId,
            @RequestParam("productSku") String productSku,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseDelete result = iOrderItem.delete(orderId,productSku,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_ITEM_PUT')")
    public ResponseEntity<ResponseSuccess> updateItem(
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderItem requestOrderItem,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseSuccess result = iOrderItem.update(orderId,requestOrderItem,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_ITEM_GET')")
    public ResponseEntity<List<OrderItemDTO>> listOrderItems(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions {
        List<OrderItemDTO> result = iOrderItem.listOrderItems(user,id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
