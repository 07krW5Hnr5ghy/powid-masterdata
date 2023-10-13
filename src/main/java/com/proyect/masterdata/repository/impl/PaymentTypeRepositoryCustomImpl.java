package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.PaymentType;
import com.proyect.masterdata.repository.PaymentTypeRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class PaymentTypeRepositoryCustomImpl implements PaymentTypeRepositoryCustom {
    @PersistenceContext(name="entityManager")
    private EntityManager entityManager;

    @Override
    public Page<PaymentType> searchForPaymentType(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<PaymentType> criteriaQuery = criteriaBuilder.createQuery(PaymentType.class);
        Root<PaymentType> itemRoot = criteriaQuery.from(PaymentType.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name,user,status,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> paymentTypeList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                paymentTypeList = listASC(sortColumn,criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                paymentTypeList = listDESC(sortColumn,criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(paymentTypeList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<PaymentType> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        long count = getOrderCount(name,user,status);
        return new PageImpl<>(orderTypedQuery.getResultList(),pageable,count);
    }

    public List<Predicate> predicateConditions(
            String name,
            String user,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<PaymentType> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();

        if(name!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("name")),name.toUpperCase())));
        }

        if(user!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("name")),user.toUpperCase())));
        }

        if(status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if(!status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<PaymentType> itemRoot
    ){
        List<Order> paymentTypeList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("NAME")){
            paymentTypeList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        if(sortColumn.equalsIgnoreCase("USER")){
            paymentTypeList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        return paymentTypeList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<PaymentType> itemRoot
    ){
        List<Order> paymentTypeList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("NAME")){
            paymentTypeList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }
        if(sortColumn.equalsIgnoreCase("USER")){
            paymentTypeList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        return paymentTypeList;
    }

    private long getOrderCount(String name,String user,Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<PaymentType> itemRoot = criteriaQuery.from(PaymentType.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name,user,status,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
