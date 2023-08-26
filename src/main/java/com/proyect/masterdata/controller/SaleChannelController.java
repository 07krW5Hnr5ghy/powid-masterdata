package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISaleChannel;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/sale-channel")
@AllArgsConstructor
public class SaleChannelController {
    private final ISaleChannel iSaleChannel;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSaleChannel.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/sale-channels")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<String> names,@RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iSaleChannel.saveAll(names,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<SaleChannelDTO> update(
            @RequestBody() RequestSaleChannel requestSaleChannel
    ) throws BadRequestExceptions {
        SaleChannelDTO result = iSaleChannel.update(requestSaleChannel);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iSaleChannel.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("/list")
    public ResponseEntity<List<SaleChannelDTO>> listSaleChannel() throws BadRequestExceptions {
        List<SaleChannelDTO> result = iSaleChannel.listSaleChannel();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse")
    public ResponseEntity<List<SaleChannelDTO>> listStatusFalse() throws BadRequestExceptions {
        List<SaleChannelDTO> result = iSaleChannel.listStatusFalse();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<SaleChannelDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        SaleChannelDTO result = iSaleChannel.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
