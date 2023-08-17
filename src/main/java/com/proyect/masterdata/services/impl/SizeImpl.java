package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.request.RequestSizeSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.SizeMapper;
import com.proyect.masterdata.repository.SizeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISize;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class SizeImpl implements ISize {
    private final SizeRepository sizeRepository;
    private final SizeMapper sizeMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name,String user,Long codeSizeType) throws BadRequestExceptions {
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            sizeRepository.save(sizeMapper.sizeToName(RequestSizeSave.builder()
                    .codeSizeType(codeSizeType)
                    .name(name.toUpperCase())
                    .user(user.toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user,Long codeSizeType) throws BadRequestExceptions{
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            List<RequestSizeSave> sizeSaves = names.stream().map(data -> RequestSizeSave.builder()
                    .user(user.toUpperCase())
                    .codeSizeType(codeSizeType)
                    .name(data.toUpperCase())
                    .build()).toList();
            sizeRepository.saveAll(sizeMapper.listSizeToListName(sizeSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public SizeDTO update(RequestSize requestSize) throws BadRequestExceptions {
        User datauser = userRepository.findById(requestSize.getUser().toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            requestSize.setName(requestSize.getName().toUpperCase());
            requestSize.setUser(requestSize.getUser().toUpperCase());
            Size size = sizeMapper.requestSizeToSize(requestSize);
            size.setDateRegistration(new Date(System.currentTimeMillis()));
            return sizeMapper.sizeToSizeDTO(sizeRepository.save(size));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            sizeRepository.deleteByIdAndUser(code,user);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            codes.stream().forEach(data -> {
                sizeRepository.deleteByIdAndUser(data,user);
            });
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<SizeDTO> list() throws BadRequestExceptions{
        try {
            return sizeMapper.listSizeToListSizeDTO(sizeRepository.findAllByStatusTrue());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    public List<SizeDTO> listStatusFalse() throws BadRequestExceptions{
        try {
            return sizeMapper.listSizeToListSizeDTO(sizeRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public SizeDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return sizeMapper.sizeToSizeDTO(sizeRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public SizeDTO findByName(String name) throws BadRequestExceptions{
        try {
            return sizeMapper.sizeToSizeDTO(sizeRepository.findByNameAndStatusTrue(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<SizeDTO> findByUser(String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user.toUpperCase()).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            return sizeMapper.listSizeToListSizeDTO(sizeRepository.findByUser(user.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<SizeDTO> findAllSizeTypeId(Long codeSizeType) throws BadRequestExceptions {
        try {
            return sizeMapper.listSizeToListSizeDTO(sizeRepository.findAllByStatusTrueAndSizeTypeId(codeSizeType));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<SizeDTO> findAllSizeTypeName(String nameSizeType) throws BadRequestExceptions {
        try {
            return sizeMapper.listSizeToListSizeDTO(sizeRepository.findAllByStatusTrueAndSizeTypeName(nameSizeType));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
