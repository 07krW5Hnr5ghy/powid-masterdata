package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderReturnItem;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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
    ) throws BadRequestExceptions {
        List<OrderReturnItemDTO> result = iOrderReturnItem.list(user,orderId);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
