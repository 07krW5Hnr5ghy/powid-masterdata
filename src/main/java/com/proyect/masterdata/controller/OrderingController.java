package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrdering;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("ordering")
@AllArgsConstructor
public class OrderingController {

    private final IOrdering iOrdering;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestOrderSave requestOrderSave,
            @RequestParam("tokenUser") String tokenUser
    ) throws InternalErrorExceptions, BadRequestExceptions{
        ResponseSuccess result = iOrdering.save(requestOrderSave,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<OrderDTO>> list(
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions{
        Page<OrderDTO> result = iOrdering.list(orderId,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
