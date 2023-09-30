package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestModule;
import com.proyect.masterdata.dto.request.RequestModuleSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IModule;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/modules")
@AllArgsConstructor
public class ModuleController {
    private IModule iModule;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("price") double price,
            @RequestParam("statusModule") int statusModule,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iModule.save(name,price,statusModule,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/modules",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestModuleSave> modules,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iModule.saveAll(modules,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ModuleDTO> update(
            @RequestBody() RequestModule requestModule
    ) throws BadRequestExceptions {
        ModuleDTO result = iModule.update(requestModule);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iModule.delete(code,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/list",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ModuleDTO>> listModule() throws BadRequestExceptions {
        List<ModuleDTO> result = iModule.listModule();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ModuleDTO>> list(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<ModuleDTO> result = iModule.list(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ModuleDTO>> listStatusFalse(
            @RequestParam(value = "name",required = false) String name,
            @RequestParam(value = "user",required = false) String user,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<ModuleDTO> result = iModule.listStatusFalse(name,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ModuleDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ModuleDTO result = iModule.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
