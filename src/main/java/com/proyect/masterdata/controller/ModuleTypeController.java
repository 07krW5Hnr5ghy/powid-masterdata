package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ModuleTypeDTO;
import com.proyect.masterdata.dto.request.RequestModuleTypeSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IModuleType;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/module-types")
@AllArgsConstructor
public class ModuleTypeController {
    private IModuleType iModuleType;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("userType") String userType,
            @RequestParam("module") String module,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iModuleType.save(userType,module,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "modules-types",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestParam("userType") String userType,
            @RequestBody() List<String> moduleList,
            @RequestParam("user") String user
    ) throws BadRequestExceptions{
        ResponseSuccess result = iModuleType.saveAll(userType,moduleList,user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ModuleTypeDTO>> list(
            @RequestParam(value = "userType",required = false) String userType,
            @RequestParam(value = "module",required = false) String module,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<ModuleTypeDTO> result = iModuleType.list(userType,module,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
