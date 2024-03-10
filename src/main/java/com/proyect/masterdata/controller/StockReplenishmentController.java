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

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-replenishment")
@AllArgsConstructor
public class StockReplenishmentController {
    private final IStockReplenishment iStockReplenishment;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_POST')")
    private ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") Long orderId,
            @RequestBody() List<RequestStockReplenishmentItem> requestStockReplenishmentItems,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iStockReplenishment.save(orderId,requestStockReplenishmentItems,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_GET')")
    private ResponseEntity<Page<StockReplenishmentDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "orderId",required = false) Long orderId,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<StockReplenishmentDTO> result = iStockReplenishment.list(user,orderId,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
