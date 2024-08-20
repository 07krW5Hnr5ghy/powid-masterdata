package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.OrderPaymentState;
import com.proyect.masterdata.repository.OrderPaymentStateRepositoryCustom;
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
public class OrderPaymentStateCustomImpl implements OrderPaymentStateRepositoryCustom {
    @PersistenceContext(name="entityManager")
    private EntityManager entityManager;

    @Override
    public Page<OrderPaymentState> searchForPaymentState(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status){

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<OrderPaymentState> criteriaQuery = criteriaBuilder.createQuery(OrderPaymentState.class);
        Root<OrderPaymentState> itemRoot = criteriaQuery.from(OrderPaymentState.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name,user,status,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> paymentStateList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                paymentStateList = listASC(sortColumn,criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                paymentStateList = listDESC(sortColumn,criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(paymentStateList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<OrderPaymentState> orderTypedQuery = entityManager.createQuery(criteriaQuery);
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
            Root<OrderPaymentState> itemRoot
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
            Root<OrderPaymentState> itemRoot
    ){
        List<Order> paymentStateList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("NAME")){
            paymentStateList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        if(sortColumn.equalsIgnoreCase("USER")){
            paymentStateList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        return paymentStateList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderPaymentState> itemRoot
    ){
        List<Order> paymentStateList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("NAME")){
            paymentStateList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }
        if(sortColumn.equalsIgnoreCase("USER")){
            paymentStateList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        return paymentStateList;
    }

    private long getOrderCount(String name,String user,Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<OrderPaymentState> itemRoot = criteriaQuery.from(OrderPaymentState.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name,user,status,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
