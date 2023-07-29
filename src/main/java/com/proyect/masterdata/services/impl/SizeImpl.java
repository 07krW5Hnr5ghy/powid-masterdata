package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.SizeMapper;
import com.proyect.masterdata.repository.SizeRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class SizeImpl implements IMasterList {
    private final SizeRepository sizeRepository;
    private final SizeMapper sizeMapper;


    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return sizeMapper.sizeListToSizeListDTO(sizeRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            sizeRepository.save(Size.builder().name(name).status(true).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            Size record = sizeRepository.findById(id).get();
            sizeRepository.save(Size.builder()
                    .name(record.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(record.getId())
                    .status(false)
                    .build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public MasterListDTO updateRecord(String name, Long id) throws BadRequestExceptions {
        try{
            Size size = sizeRepository.save(Size.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build()
            );
            return sizeMapper.INSTANCE.INSTANCE.sizeToSizeDTO(size);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}
